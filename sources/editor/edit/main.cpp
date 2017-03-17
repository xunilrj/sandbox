
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
CHAR_INFO chiBuffer[80 * 10];
void OpenFile()
{
	std::ifstream myfile;
	myfile.open("example.txt");

	SMALL_RECT srctReadRect;
	SMALL_RECT srctWriteRect;
	// [2][80]; 
	COORD coordBufSize;
	COORD coordBufCoord;
	BOOL fSuccess;



	HANDLE hStdout, hNewScreenBuffer;
	hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	hNewScreenBuffer = CreateConsoleScreenBuffer(
		GENERIC_READ |           // read/write access 
		GENERIC_WRITE,
		FILE_SHARE_READ |
		FILE_SHARE_WRITE,        // shared 
		NULL,                    // default security attributes 
		CONSOLE_TEXTMODE_BUFFER, // must be TEXTMODE 
		NULL);

	SetConsoleActiveScreenBuffer(hNewScreenBuffer);

	coordBufSize.Y = 10;
	coordBufSize.X = 80;
	coordBufCoord.X = 0;
	coordBufCoord.Y = 0;

	srctWriteRect.Top = 10;    // top lt: row 10, col 0 
	srctWriteRect.Left = 0;
	srctWriteRect.Bottom = 12; // bot. rt: row 11, col 79 
	srctWriteRect.Right = 79;

	int x = 0, y = 0;
	while (myfile.good())
	{
		auto readchar = (char)myfile.get();
		if (readchar == -1) {
			break;
		}
		else  if (readchar == 10) {
			y++;
			x = 0;
			continue;
		}
		else {
			chiBuffer[(y * 80) + x].Char.AsciiChar = readchar;
			chiBuffer[(y * 80) + x].Attributes = FOREGROUND_BLUE | FOREGROUND_GREEN | FOREGROUND_RED;
			++x;
		}
	}

	WriteConsoleOutput(
		hNewScreenBuffer, // screen buffer to write to 
		chiBuffer,        // buffer to copy from 
		coordBufSize,     // col-row size of chiBuffer 
		coordBufCoord,    // top left src cell in chiBuffer 
		&srctWriteRect);

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
	std::cout << "Open(F1)";

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
				OpenFile();
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