
#include <iostream>
#include <fstream>
#include <Windows.h>

BOOL setxy(int x, int y)
{
	COORD c = { x,y };
	return SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE), c);
}

void DrawRect(int x, int y, int width, int height, int curPosX = 0, int curPosY = 0)
{
	setxy(x, y);std::cout << char(201);
	for (int i = 1; i < width; i++)std::cout << char(205);
	std::cout << char(187);
	setxy(x, height + y);std::cout << char(200);
	for (int i = 1; i < width; i++)std::cout << char(205);
	std::cout << char(188);
	for (int i = y + 1; i < height + y; i++)
	{
		setxy(x, i);std::cout << char(186);
		setxy(x + width, i);std::cout << char(186);
	}
	setxy(curPosX, curPosY);
}

void SaveFile()
{
	std::ofstream myfile;
	myfile.open("example.txt");
	myfile << "Writing this to a file.\n";
	myfile.close();
}

int main(int argc, char**argv)
{
	DWORD        mode;
	INPUT_RECORD event;
	BOOL         QUIT = FALSE;
	HANDLE hstdin = GetStdHandle(STD_INPUT_HANDLE);

	GetConsoleMode(hstdin, &mode);
	SetConsoleMode(hstdin, ENABLE_WINDOW_INPUT);

	DrawRect(0, 0, 120, 2, 0, 0);
	setxy(1, 1);
	std::cout << "Save(F1)";
	
	DrawRect(0, 40, 120, 2, 0, 0);

	int cx = 1, cy = 41;
	bool inserting = false;

	while (!QUIT) {
		DWORD count;
		ReadConsoleInput(hstdin, &event, 1, &count);
		setxy(80, 1);
		std::cout << "Inserting:" << inserting;

		setxy(100, 1);
		std::cout << "Key Code   = " << event.Event.KeyEvent.wVirtualKeyCode << " \n";

		setxy(cx, cy);

		if ((event.EventType == KEY_EVENT) && !event.Event.KeyEvent.bKeyDown) {
			switch (event.Event.KeyEvent.wVirtualKeyCode) {
				case VK_ESCAPE:
					QUIT = TRUE;
					break;
				case VK_INSERT:
					inserting = !inserting;
					if (inserting) {
						cx = 1;
						cy = 41;
					}
					break;
				case VK_F1:
					SaveFile();
					break;
				case VK_LEFT:
					cx--;
					break;
				case VK_RIGHT:
					cx++;
					break;
				case VK_UP:
					cy--;
					break;
				case VK_DOWN:
					cy++;
					break;
				default:
					std::cout << event.Event.KeyEvent.uChar.AsciiChar;
					cx++;
					break;
				/*case VK_F2:
					refresh();
					break;
				case VK_F5:
					break;
				case VK_F8:
					break;*/
			}

			event.Event.KeyEvent.wVirtualKeyCode = -1;
		}
	}

	SetConsoleMode(hstdin, mode);


}