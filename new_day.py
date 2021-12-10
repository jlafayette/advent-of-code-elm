import sys
from pathlib import Path


def main(day: int):
    day_text = Path("DayTemplate.elm").read_text()
    day_text = day_text.replace("module Day0", f"module Day{day}", 1)
    day_text = day_text.replace("import Day0Input", f"import Day{day}Input", 1)
    Path(f"src/Day{day}.elm").write_text(day_text)

    input_text = Path("DayInputTemplate.elm").read_text()
    input_text = input_text.replace("module Day0Input", f"module Day{day}Input", 1)
    Path(f"src/Day{day}Input.elm").write_text(input_text)


if __name__ == '__main__':
    main(sys.argv[1])

