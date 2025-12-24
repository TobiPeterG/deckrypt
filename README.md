# Deckrypt

Translate game controller inputs into keyboard events.

Deckrypt is designed for users who desire full disk encryption on devices like the Steam Deck. It leverages a Rust-based application to translate game controller inputs into keyboard events, enabling the unlocking of encrypted partitions through specific controller combinations or traditional password entry.

## Table of Contents

- [Deckrypt](#deckrypt)
  - [Table of Contents](#table-of-contents)
  - [Features](#features)
  - [How It Works](#how-it-works)
  - [Combinations](#combinations)
    - [Input Mappings](#input-mappings)
      - [Mapping Structure](#mapping-structure)
      - [Example Mapping](#example-mapping)
    - [Combination Structure](#combination-structure)
    - [Sequence Formation](#sequence-formation)
    - [The Enter key](#the-enter-key)
  - [Security](#security)
    - [Mapping Complexity](#mapping-complexity)
    - [Entropy Calculation](#entropy-calculation)
  - [Installation](#installation)
  - [Configuration](#configuration)
    - [Creating Configuration Files](#creating-configuration-files)
    - [Supported categories](#supported-categories)
  - [Automatic Mapping](#automatic-mapping)
  - [Manual building](#manual-building)
    - [Prerequisites](#prerequisites)
    - [Building from Source](#building-from-source)
  - [License](#license)
  - [Contributing](#contributing)
  - [Credits](#credits)

## Features

- **Game Controller Integration:** Use game controllers to unlock encrypted partitions.
- **Automatic and Manual Mapping:** Automatically assign controller inputs to keyboard events or manually configure mappings.
- **Friendly Logging:** Optional friendly names for easier identification of controller inputs.
- **Flexible Configuration:** Support for multiple game controllers with individual configuration files.
- **Security:** Combines multiple controller inputs to create secure unlocking sequences.

## How It Works

Deckrypt operates by mapping gamepad inputs to keyboard events. When a specific combination of buttons and axes on the controller is detected, Deckrypt generates corresponding keyboard events as if the sequence were typed on a physical keyboard. This sequence can then be added to a LUKS slot, allowing for secure unlocking of the encrypted partition using either the controller combination or a traditional password.

A **dracut hook** ensures that Deckrypt operates in early userspace, enabling it to unlock the root partition during the boot process.

## Combinations

In Deckrypt, combinations are the fundamental units used to unlock a LUKS-encrypted root partition. Each combination translates specific gamepad inputs into keyboard events, allowing you to authenticate using your game controller. This section explains how combinations are structured and how they contribute to the overall security of your encrypted system.

### Input Mappings

Deckrypt employs a flexible mapping system where each gamepad input can represent multiple values based on active modifiers. This system enhances both usability and security by providing multiple layers of input interpretation.

#### Mapping Structure

Each gamepad input (buttons and axes) can be mapped in the following ways:

1. **Normal Mapping:** The default mapping when no modifiers are active.
2. **Alternate Mapping:** An alternative mapping activated by the Alternate modifier.
3. **Shift Modifier:** An additional modifier (like the Shift key) that changes the mapping when active (`a` becomes `A`, `1` becomes `!`).
4. **Shift + Alternate:** Both Shift and Alternate modifiers active simultaneously.

This results in four possible mappings for each gamepad input.

#### Example Mapping

Consider the following mappings for buttons:

| Gamepad Input | Normal | Shift | Alternate | Shift + Alternate |
| --- | --- | --- | --- | --- |
| BTN_A | 'a' | 'A' | '1' | '!' |
| BTN_B | 'b' | 'B' | '2' | '@' |
| BTN_X | 'x' | 'X' | '3' | '#' |
| BTN_Y | 'y' | 'Y' | '4' | '$' |

Similarly, axes directions follow the same mapping logic:

| Axis Input | Normal | Shift | Alternate | Shift + Alternate |
| --- | --- | --- | --- | --- |
| ABS_X_POS | 'd' | 'D' | '5' | '%' |
| ABS_X_NEG | 'a' | 'A' | '1' | '!' |
| ABS_Y_POS | 'w' | 'W' | '6' | '^' |
| ABS_Y_NEG | 's' | 'S' | '2' | '@' |

### Combination Structure

A combination consists of one or more gamepad inputs mapped under specific modifier states. Each combination contributes characters to an unlocking sequence, which is used to authenticate and unlock the encrypted partition.

**Components of a Combination:**

1. **Hold Inputs:**
    - **Definition:** Inputs (buttons or axes) that are pressed and held.
    - **Purpose:** Used to activate modifiers (Shift or Alternate).
    - **Example:** Holding `BTN_SHIFT` to activate the Shift modifier.

2. **Tap Input:**
    - **Definition:** A single input pressed to conclude the combination.
    - **Purpose:** Generates the corresponding character based on active modifiers.
    - **Example:** Tapping `BTN_A` to produce `a`, `A`, `1`, or `!` depending on modifier states.

### Sequence Formation

Multiple combinations can be chained together to form a secure unlocking sequence. Each combination adds its mapped character to the sequence.

**Example Sequence:**

1. **Combination 1:** Hold `BTN_SHIFT`, tap `BTN_A` → `A`
2. **Combination 2:** Tap `BTN_B` without modifiers → `b`
3. **Combination 3:** Hold `BTN_ALT`, tap `BTN_X` → `3`
4. **Combination 4:** Hold `BTN_ALT` + `BTN_SHIFT`, tap `BTN_Y`  → `$`

**Final Unlocking Sequence:**
`Ab3$`

### The Enter key

Since it is not only necessary to enter a sequence, but we also need to communicate that the sequence is complete, we need to have the possibility to press the `Enter` key. It is equally useful to have a way to remove the last character and it is useful to stop deckrypt again. To realize this, Deckrypt supports the special mapping `ENTER`, which can be assigned to a button in normal mode. If this is set, the behavior of this button differs from the other assignments as follows:

1. If this button is pressed **WITHOUT** a modifier key, an `ENTER` is emitted.
2. If the key is pressed with **ONE** of the modifier keys, a `BACKSPACE` is emitted, which removes the last character entered.
3. If the key is pressed with **BOTH** modifier keys, deckrypt is stopped.

This special behavior also means that a manual assignment for the alternate mode of this key is not possible and any manual assignment will be ignored.

## Security

Deckrypt's security is enhanced through the combination of multiple mappings and the use of modifiers, which exponentially increases the complexity and unpredictability of the unlocking sequence.

### Mapping Complexity

Each gamepad input can represent four distinct values based on the combination of modifiers, as described above.
This multiplicity ensures that each input contributes multiple bits of entropy to the unlocking sequence.

### Entropy Calculation

**Allowed Characters:**
Deckrypt utilizes a predefined set of allowed characters (`ALLOWED_CHARACTERS`), including:

- **Letters:** `a`-`z` (26)
- **Digits:** `0`-`9` (10)
- **Symbols:** `-`, `=`, `[`, `]`, `\`, `;`, `'`, `,`, `.`, `/`, ```, ... (11)
- **Shift variants:** `A`-`Z`, `!`, `@`, ... (47)

**Total Unique Characters:**

47 + 47 = 94

**Bits per Character:**

Each character provides approximately:
$\log_2(94) ≈ 6.555...$ bits

**Total Entropy:**

The total entropy of an unlocking sequence is the sum of the entropy of each character in the sequence.

**Example Calculation:**

- **Single Combination:** `A` → 1 character →
$1 \cdot 6.555 ≈ 6.555$ bits
- **Four Combinations:** `Ab3$` → 4 characters →
$4 \cdot 6.555 ≈ 26.22$ bits

**Security Implications:**

- **Higher Entropy:** Longer sequences with more characters exponentially increase the total entropy, making unauthorized access highly improbable.
- **Brute-Force Resistance:** High entropy sequences render brute-force attacks computationally infeasible within a reasonable timeframe.

By utilizing normal and alternate mappings along with modifier keys, Deckrypt ensures that each gamepad input contributes significantly to the overall security of the unlocking sequence. This design not only enhances security through increased entropy but also offers flexibility in configuring and using controller inputs for authentication.

**A sequence of 9 or 10 combinations would already provide decent security.**

## Installation

A pre-built package for openSUSE based distributions is available [here](https://build.opensuse.org/package/show/devel:microos:yuga:unstable/deckrypt)

## Configuration

Deckrypt uses configuration files to map controller inputs to keyboard events. These configurations are typically located in `/etc/deckrypt/` or `/usr/share/deckrypt` and can be customized per device.

### Creating Configuration Files

1. Start deckrypt with the -u option to show devices unknown to deckrypt and increase logging

```bash
deckrypt -u -vvvv
```

1. Select your device
2. Have a look at the automatic mapping. They show you the supported buttons and axes.
3. Deckrypt outputs how the config file would be called and where to place it. Have a look at the config files under [configs](./configs)

### Supported categories

- **Buttons:** Map each button to a character or special key.
- **Axes:** Define mappings for axis directions.
- **Alternate Mappings:** Provide alternate mappings for different modifier states.
- **Modifiers:** Define which inputs act as modifiers (Currently a "shift" action and an alternate mode are supported).
- **Friendly Names:** Optional, for more readable logs and outputs.

## Automatic Mapping

If you prefer Deckrypt to automatically assign mappings to unassigned controller inputs:

- **Enable Automatic Mapping:** Use the -m or --mapping flag when running Deckrypt.

Deckrypt will assign available controller inputs to allowed characters sequentially, ensuring no conflicts with manual mappings.

However, please note these automatic mappings might not always be stable, so different runs of deckrypt might produce different mappings!
The automatic mapping is mainly thought for debugging and initial testing of a new controller.

## Manual building

### Prerequisites

Ensure that the following dependencies are installed on your system:

- **Rust Toolchain:** For building the application.
- **libevdev:** Library for handling input events.
- **Dracut:** For initramfs generation.
- **Cargo-Packaging:** For packaging the Rust application.
- **kbd-devel:** For creating the kernel keymap
- **libxkbcommon-devel:** For creating the wayland keymap

On openSUSE, you can install the necessary dependencies using zypper:

```bash
sudo zypper install cargo cargo-packaging rust libevdev-devel kbd-devel pkgconfig dracut libxkbcommon-devel
```

### Building from Source

1. Clone the Repository:

```bash
git clone https://github.com/TobiPeterG/deckrypt.git
cd deckrypt
```

1. Build the Application

```bash
cargo build --release
```

1. Install the Binary

```bash
sudo cp target/release/deckrypt /usr/local/bin/
```

1. Install Configuration Files and Dracut Modules

```bash
sudo mkdir -p /etc/deckrypt
sudo cp configs/*.toml /etc/deckrypt/
sudo cp dracut/modules.d/50deckrypt/* /usr/lib/dracut/modules.d/50deckrypt/
```

1. rebuild initrd

```bash
dracut -f
```

## License

This project is licensed under the [GPL-3.0](LICENSE) License.

## Contributing

Contributions are welcome! Please fork the repository and submit pull requests for any enhancements or bug fixes. For major changes, please open an issue first to discuss your ideas.

## Credits

This is based on the idea of [pmkap/deckrypt](https://github.com/pmkap/deckrypt).
