#include <pthread.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>
#include <getopt.h>
#include <libevdev/libevdev.h>

#include "uitype.h"

// Threshold after a button becomes a hold-button (milliseconds)
#define THRESHOLD 250

// Set this to fit the longest name in the buttons struct below
#define MAX_NAME_LEN 5

static bool verbose = false;

typedef struct Button
{
    // linux event code
    uint16_t code;
    char name[MAX_NAME_LEN + 1];
    bool pressed;
    int64_t time_pressed;
    int64_t time_released;
} Button;

typedef struct Axis
{
    // linux event code
    uint16_t code;
    // An axis can be "translated" to two buttons. E.g. the horizontal D-pad axis
    // to buttons for left and right. These two buttons correspond to X and Y here.
    char nameX[MAX_NAME_LEN + 1];
    char nameY[MAX_NAME_LEN + 1];
    // press
    int16_t valueX;
    int16_t valueY;
    // release
    int16_t value0;
} Axis;

// Use evtest to find which events are supported by a controller.
// Buttons not listed here are ignored.
#define ENTER_BUTTON BTN_BASE4
#define CLEAR_BUTTON BTN_BASE3
static Button buttons[] = {
    {.code = BTN_TRIGGER, .name = "TRIG", .pressed = false},
    {.code = BTN_THUMB, .name = "THMB", .pressed = false},
    {.code = BTN_THUMB2, .name = "THMB2", .pressed = false},
    {.code = BTN_TOP, .name = "TOP", .pressed = false},
    {.code = BTN_TOP2, .name = "TOP2", .pressed = false},
    {.code = BTN_PINKIE, .name = "PINK", .pressed = false},
    {.code = BTN_BASE, .name = "BASE", .pressed = false},
    {.code = BTN_BASE2, .name = "BASE2", .pressed = false},
    {.code = BTN_BASE5, .name = "BASE5", .pressed = false},
    {.code = BTN_BASE6, .name = "BASE6", .pressed = false},
    // These "buttons" are translated from the axes below, names have to match
    {.code = 0xffff, .name = "D-RI", .pressed = false},
    {.code = 0xffff, .name = "D-LE", .pressed = false},
    {.code = 0xffff, .name = "D-UP", .pressed = false},
    {.code = 0xffff, .name = "D-DO", .pressed = false},
    {.code = 0xffff, .name = "L-RI", .pressed = false},
    {.code = 0xffff, .name = "L-LE", .pressed = false},
    {.code = 0xffff, .name = "L-UP", .pressed = false},
    {.code = 0xffff, .name = "L-DO", .pressed = false},
    {.code = 0xffff, .name = "R-RI", .pressed = false},
    {.code = 0xffff, .name = "R-LE", .pressed = false},
    {.code = 0xffff, .name = "R-UP", .pressed = false},
    {.code = 0xffff, .name = "R-DO", .pressed = false},
    {.code = 0xffff, .name = "L-TRIG", .pressed = false},
    {.code = 0xffff, .name = "R-TRIG", .pressed = false},
    // Support Gamepads
    {.code = BTN_SOUTH, .name = "A", .pressed = false},
    {.code = BTN_EAST, .name = "B", .pressed = false},
    {.code = BTN_NORTH, .name = "X", .pressed = false},
    {.code = BTN_WEST, .name = "Y", .pressed = false},
    {.code = BTN_TL, .name = "LB", .pressed = false},
    {.code = BTN_TR, .name = "RB", .pressed = false},
    {.code = BTN_SELECT, .name = "SEL", .pressed = false},
    {.code = BTN_START, .name = "START", .pressed = false},
    {.code = BTN_MODE, .name = "MODE", .pressed = false},
    {.code = BTN_THUMBL, .name = "LTHMB", .pressed = false},
    {.code = BTN_THUMBR, .name = "RTHMB", .pressed = false},
};

static Axis axes[] = {
    {.code = ABS_X,
     .nameX = "L-LE",
     .nameY = "L-RI",
     .valueX = -32768,
     .value0 = 0,
     .valueY = 32767},
    {.code = ABS_Y,
     .nameX = "L-UP",
     .nameY = "L-DO",
     .valueX = -32768,
     .value0 = 0,
     .valueY = 32767},
    {.code = ABS_Z,
     .nameX = "L-TRIG", // Left trigger
     .nameY = "",       // No associated button for Y-axis on this axis
     .valueX = 0,
     .value0 = 128,
     .valueY = 255},
    {.code = ABS_RX,
     .nameX = "R-LE",
     .nameY = "R-RI",
     .valueX = -32768,
     .value0 = 0,
     .valueY = 32767},
    {.code = ABS_RY,
     .nameX = "R-UP",
     .nameY = "R-DO",
     .valueX = -32768,
     .value0 = 0,
     .valueY = 32767},
    {.code = ABS_RZ,
     .nameX = "R-TRIG", // Right trigger
     .nameY = "",       // No associated button for Y-axis on this axis
     .valueX = 0,
     .value0 = 128,
     .valueY = 255},
    {.code = ABS_HAT0X,
     .nameX = "D-LE",
     .nameY = "D-RI",
     .valueX = -1,
     .value0 = 0,
     .valueY = 1},
    {.code = ABS_HAT0Y,
     .nameX = "D-UP",
     .nameY = "D-DO",
     .valueX = -1,
     .value0 = 0,
     .valueY = 1}};

static const size_t n_buttons = sizeof(buttons) / sizeof(Button);
static const size_t n_axes = sizeof(axes) / sizeof(Axis);

static struct timeval start_time;

// Convert the timestamp of an event to milliseconds since the program started
static int64_t time2millis(struct timeval t)
{
    int64_t secs_diff = t.tv_sec - start_time.tv_sec;
    int64_t usecs_diff;
    if (t.tv_usec >= start_time.tv_usec)
    {
        usecs_diff = t.tv_usec - start_time.tv_usec;
    }
    else
    {
        usecs_diff = 1000000 - (start_time.tv_usec - t.tv_usec);
        secs_diff--;
    }
    return secs_diff * 1000 + usecs_diff / 1000;
}

static Button *button_from_code(uint16_t code)
{
    for (size_t i = 0; i < n_buttons; i++)
    {
        if (code == buttons[i].code)
        {
            return &buttons[i];
        }
    }
    return NULL;
}

static Button *button_from_name(char *name)
{
    for (size_t i = 0; i < n_buttons; i++)
    {
        if (strncmp(name, buttons[i].name, MAX_NAME_LEN) == 0)
        {
            return &buttons[i];
        }
    }
    return NULL;
}

// Argument Button *b: The released button that concludes the combination
static void combination(Button *b)
{
    char buffer[n_buttons * (MAX_NAME_LEN + 1)];
    buffer[0] = '\0';

    // Check for hold-buttons
    // A button becomes a hold-button if it
    // * is pressed for at least the threshold time, and
    // * was pressed before the released button b.
    for (size_t i = 0; i < n_buttons; i++)
    {
        if (buttons[i].pressed &&
            b->time_released - buttons[i].time_pressed > THRESHOLD &&
            b->time_pressed > buttons[i].time_pressed)
        {
            if (strnlen(buffer, 1) == 0)
            {
                strcat(buffer, buttons[i].name);
            }
            else
            {
                strcat(buffer, "+");
                strcat(buffer, buttons[i].name);
            }
        }
    }

    if (strnlen(buffer, 1) == 0)
    {
        strcat(buffer, b->name);
    }
    else
    {
        strcat(buffer, "+");
        strcat(buffer, b->name);
    }

    strcat(buffer, "-");
    uitype_type(buffer);
}

static void handle_button(struct input_event *ev)
{
    Button *button = button_from_code(ev->code);
    if (button == NULL)
    {
        if (verbose)
            printf("Warning: Unknown button code %d\n", ev->code);
        return;
    }

    if (ev->value == 1)
    {
        button->pressed = true;
        button->time_pressed = time2millis(ev->time);
    }
    else if (ev->value == 0)
    {
        button->pressed = false;
        button->time_released = time2millis(ev->time);
        if (button->time_released - button->time_pressed < THRESHOLD)
        {
            combination(button);
        }
    }
}

static void handle_axis(struct input_event *ev)
{
    static bool axis_locked = false;   // Tracks whether the axis is in use
    static Button *last_button = NULL; // Tracks the last button pressed from the axis

    for (size_t i = 0; i < n_axes; i++)
    {
        if (ev->code == axes[i].code)
        {
            if (ev->value == axes[i].value0) // Axis has returned to neutral (value0)
            {
                axis_locked = false; // Unlock the axis
                if (last_button != NULL && last_button->pressed)
                {
                    // Release the button that was pressed
                    last_button->pressed = false;
                    last_button->time_released = time2millis(ev->time);
                    if (last_button->time_released - last_button->time_pressed < THRESHOLD)
                    {
                        combination(last_button);
                    }
                    last_button = NULL; // Reset the last button
                }
            }
            else if (!axis_locked) // Axis has moved, and it is not locked
            {
                Button *button = NULL;

                // Check if moving along the X or Y axis
                if (ev->value == axes[i].valueX)
                {
                    button = button_from_name(axes[i].nameX);
                }
                else if (ev->value == axes[i].valueY)
                {
                    button = button_from_name(axes[i].nameY);
                }

                if (button != NULL)
                {
                    // Register the button press
                    button->pressed = true;
                    button->time_pressed = time2millis(ev->time);
                    axis_locked = true;   // Lock the axis to prevent further presses until released
                    last_button = button; // Track the last button pressed from the axis
                }
                else
                {
                    if (verbose)
                        printf("Warning: Unknown axis code %d\n", ev->code);
                }
            }
            break;
        }
    }
}

static int is_event_device(const struct dirent *dir)
{
    return strncmp("event", dir->d_name, 5) == 0;
}

static bool find_device(struct libevdev **device)
{
    struct dirent **namelist;
    int n_devices = scandir("/dev/input/", &namelist, is_event_device, NULL);

    if (verbose)
        printf("Number of devices found: %d\n", n_devices);

    for (int i = 0; i < n_devices; i++)
    {
        char *device_path = calloc(strlen("/dev/input/") + namelist[i]->d_reclen + 1,
                                   sizeof(char));
        sprintf(device_path, "%s%s", "/dev/input/", namelist[i]->d_name);

        if (verbose)
            printf("Trying device: %s\n", device_path);

        int fd = open(device_path, O_RDONLY);
        free(device_path);

        if (libevdev_new_from_fd(fd, device) == 0)
        {
            if (verbose)
                printf("Device name: %s\n", libevdev_get_name(*device));

            if (libevdev_has_event_code(*device, EV_KEY, BTN_SOUTH))
            {
                printf("Gamepad detected: %s\n", libevdev_get_name(*device));
                for (int j = i; j < n_devices; j++)
                {
                    free(namelist[j]);
                }
                free(namelist);
                return true;
            }
            else
            {
                if (verbose)
                    printf("No gamepad buttons found on this device.\n");
                libevdev_free(*device);
                *device = NULL;
            }
        }
        else
        {
            printf("Failed to open device: %s\n", namelist[i]->d_name);
        }
        close(fd);
        free(namelist[i]);
    }
    free(namelist);
    if (verbose)
        printf("No suitable device found.\n");
    return false;
}

static volatile sig_atomic_t terminate = 0;

void signal_handler(int sig)
{
    terminate = 1; // Set flag to terminate
}

void setup_signal_handling()
{
    // Set up signal handling
    struct sigaction sa;
    sa.sa_handler = signal_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGINT, &sa, NULL);
    sigaction(SIGTERM, &sa, NULL);
}

void parse_arguments(int argc, char *argv[])
{
    int opt;
    struct option long_options[] = {
        {"verbose", no_argument, 0, 'v'},
        {0, 0, 0, 0}};

    while ((opt = getopt_long(argc, argv, "v", long_options, NULL)) != -1)
    {
        switch (opt)
        {
        case 'v':
            verbose = true;
            break;
        default:
            fprintf(stderr, "Usage: %s [-v|--verbose]\n", argv[0]);
            exit(EXIT_FAILURE);
        }
    }
}

int main(int argc, char *argv[])
{
    parse_arguments(argc, argv);
    gettimeofday(&start_time, NULL);

    if (uitype_init() != 0)
    {
        return 1;
    }

    setup_signal_handling(); // Setup signal handling for SIGINT and SIGTERM

    struct libevdev *device = NULL;
    int fd;

    sigset_t sigset;
    sigemptyset(&sigset);
    sigaddset(&sigset, SIGINT);
    sigaddset(&sigset, SIGTERM);

    // Block signals in this thread so we can catch them in pselect
    pthread_sigmask(SIG_BLOCK, &sigset, NULL);

    while (!terminate)
    {
        if (device == NULL)
        {
            if (!find_device(&device))
            {
                usleep(100000); // Sleep for 100 milliseconds before trying again
            }
            else
            {
                fd = libevdev_get_fd(device); // Get file descriptor of the device
            }
        }
        else
        {
            fd_set read_fds;
            FD_ZERO(&read_fds);
            FD_SET(fd, &read_fds);

            struct timespec timeout;
            timeout.tv_sec = 0;
            timeout.tv_nsec = 100000000; // 10ms

            // Use pselect to wait for an event or signal
            int result = pselect(fd + 1, &read_fds, NULL, NULL, &timeout, &sigset);

            if (result == -1)
            {
                if (errno == EINTR) // Interrupted by signal
                {
                    if (terminate)
                        break;
                }
                else
                {
                    perror("pselect failed");
                    break;
                }
            }
            else if (result > 0 && FD_ISSET(fd, &read_fds))
            {
                struct input_event ev;
                int rc = libevdev_next_event(device, LIBEVDEV_READ_FLAG_BLOCKING, &ev);

                if (verbose)
                    printf("Event received: %d\n", rc);

                if (rc == 0)
                {
                    switch (ev.type)
                    {
                    case EV_KEY:
                        if (ev.value == 0)
                        {
                            if (ev.code == ENTER_BUTTON)
                            {
                                uitype_enter();
                            }
                            else if (ev.code == CLEAR_BUTTON)
                            {
                                uitype_ctrlu();
                            }
                        }
                        handle_button(&ev);
                        break;
                    case EV_ABS:
                        handle_axis(&ev);
                        break;
                    }
                    usleep(50000); // Avoid high CPU usage
                }
            }
        }
    }

    // Cleanup
    if (device != NULL)
    {
        libevdev_free(device);
    }
    uitype_deinit();

    return 0;
}