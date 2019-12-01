#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <regex>

void replace(std::string& data, 
    const std::string& toSearch,
    const std::string& replaceStr)
{
	// Get the first occurrence
	size_t pos = data.find(toSearch);
 
	// Repeat till end is reached
	while( pos != std::string::npos)
	{
		// Replace this occurrence of Sub String
		data.replace(pos, toSearch.size(), replaceStr);
		// Get the next occurrence from the current position
		pos =data.find(toSearch, pos + replaceStr.size());
	}
}

bool startsWith(const std::string& str, const std::string& prefix)
{
    auto r = std::mismatch(prefix.begin(), prefix.end(), str.begin());
    return r.first == prefix.end();
}

inline std::ostream& defaultColor(std::ostream &s) { return std::cout << "\033[0m"; }
inline std::ostream& red(std::ostream &s) { return std::cout << "\033[31m"; }
inline std::ostream& green(std::ostream &s) { return std::cout << "\033[32m"; }

#ifdef _WIN32
    #include <Windows.h>        
#endif
int main(int argc, char** argv)
{
    #ifdef _WIN32
        //SetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE), ENABLE_VIRTUAL_TERMINAL_PROCESSING);
    #endif

    auto testName = argv[1];
    std::cout << "Using test file: " << testName << std::endl;

    std::ifstream infile{testName};
    std::string line;

    std::regex testRegex("^#TEST,(.*?)#$",
            std::regex_constants::ECMAScript | std::regex_constants::icase);
    std::regex assertRegex("^#ASSERT,(.*?)#$",
            std::regex_constants::ECMAScript | std::regex_constants::icase);
    std::smatch matches;
    while (std::getline(infile, line))
    {
        //std::cout << line << std::endl;
        replace(line, "#NEWLINE#", "\n");
        if(line[0] == '<')
        {
            auto actual = line.substr(2, line.size() - 2);
            //std::cout << "WRITE" << std::endl << "[" << actual << "]" << std::endl;
        }
        else if(line[0] == '>')
        {
            auto expected = line.substr(2, line.size() - 2);
            //std::cout << "READ & ASSERT" << std::endl << "[" << actual << "]" << std::endl;
            std::string actual;
            std::cin >> actual;

            if(actual == expected)
            {
                green(std::cout) << "OK" << std::endl;
                defaultColor(std::cout);
            }
            else
            {
                red(std::cout) << "ERROR" << std::endl;
                defaultColor(std::cout) << "Expected: [" << expected << "]" << std::endl;
                std::cout << "Actual:   [" << actual << "]" << std::endl;
            }
            
        }
        else if (std::regex_search(line, matches, testRegex))
        {
            auto testName = matches[1].str();
            std::cout << "TEST: " << testName << std::endl;
        }
        else if (std::regex_search(line, matches, assertRegex))
        {
            auto testName = matches[1].str();
            std::cout << "ASSERT: " << testName << std::endl;
        }
    }
    return 0;
}