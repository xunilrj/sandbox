#include <iostream>
#include <stack>
#include <string>
#include <regex>
#include <tuple>

struct Bracket {
    Bracket(char type, int position):
        type(type),
        position(position)
    {}

    bool Matchc(char c) {
        if (type == '[' && c == ']')
            return true;
        if (type == '{' && c == '}')
            return true;
        if (type == '(' && c == ')')
            return true;
        return false;
    }

    char type;
    int position;
};

std::tuple<bool,int> IsBracketMatched(std::string text)
{
    std::stack<Bracket> opening_brackets_stack;
    const char open[] = "([{";
    const char close[] = ")]}";
    auto expected_open = [&](char closec){
        auto begin = std::begin(close);
        auto pos = std::find(begin, std::end(close), closec);
        auto index  = pos - begin;
        return open[index];
    };
    for (int position = 0; position < text.length(); ++position) {
        char next = text[position];

        // std::cout << next;
        
        if (next == '(' || next == '[' || next == '{') {
            // std::cout << " +";
            opening_brackets_stack.emplace(next,position);
        }

        if (next == ')' || next == ']' || next == '}') {
            // std::cout << " -";

            if(opening_brackets_stack.size() == 0)
            {
                return std::make_tuple(false,position+1);
            }

            auto top = opening_brackets_stack.top();
            if(top.type == expected_open(next))
            {
                opening_brackets_stack.pop();
            }
            else
            {
                // std::cout << " expected " << expected_open(next);
                return std::make_tuple(false,position+1);
            }
        }

        // std::cout << std::endl;
    }

    // std::cout << "ok";

    if(opening_brackets_stack.size() == 0)
    {
        return std::make_tuple(true,0);
    }
    else
    {
        return std::make_tuple(false,opening_brackets_stack.top().position+1);
    }
}

#ifdef UNITTESTS

#define CATCH_CONFIG_MAIN
#include "../../catch.hpp"

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

template <typename TOut>
void ls(const std::string &directory, const std::string &pattern, TOut out)
{
#ifdef WINDOWS
    HANDLE dir;
    WIN32_FIND_DATA file_data;

    if ((dir = FindFirstFile((directory + "/*").c_str(), &file_data)) == INVALID_HANDLE_VALUE)
        return; /* No files found */

    do {
        const string file_name = file_data.cFileName;
        const string full_file_name = directory + "/" + file_name;
        const bool is_directory = (file_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0;

        if (file_name[0] == '.')
            continue;

        if (is_directory)
            continue;

        out(full_file_name);
    } while (FindNextFile(dir, &file_data));

    FindClose(dir);
#else
    auto regex_pattern = std::regex(pattern);

    DIR *dir;
    class dirent *ent;
    class stat st;

    dir = opendir(directory.c_str());
    while ((ent = readdir(dir)) != NULL) {
        const std::string file_name = ent->d_name;
        const std::string full_file_name = directory + "/" + file_name;

        if (file_name[0] == '.')
            continue;

        if (stat(full_file_name.c_str(), &st) == -1)
            continue;

        const bool is_directory = (st.st_mode & S_IFDIR) != 0;

        if (is_directory)
            continue;

        if (std::regex_match (full_file_name, regex_pattern))
        {
            out(full_file_name);
        }
    }
    closedir(dir);
#endif
} // GetFilesInDirectory

std::istream& safeGetline(std::istream& is, std::string& t)
{
    t.clear();

    // The characters in the stream are read one-by-one using a std::streambuf.
    // That is faster than reading them one-by-one using the std::istream.
    // Code that uses streambuf this way must be guarded by a sentry object.
    // The sentry object performs various tasks,
    // such as thread synchronization and updating the stream state.

    std::istream::sentry se(is, true);
    std::streambuf* sb = is.rdbuf();

    for(;;) {
        int c = sb->sbumpc();
        switch (c) {
        case '\n':
            return is;
        case '\r':
            if(sb->sgetc() == '\n')
                sb->sbumpc();
            return is;
        case EOF:
            // Also handle the case when the last line has no line ending
            if(t.empty())
                is.setstate(std::ios::eofbit);
            return is;
        default:
            t += (char)c;
        }
    }
}

std::string readAllText(const std::string & path)
{
    auto f = std::ifstream(path.c_str());
    std::string line;
    safeGetline(f, line);
    return line;
}

void runFileTestsCases()
{
    ls("./tests", "^./tests/\\d\\d$", [](auto& filename){
        auto exp = readAllText(filename);
        auto answer = IsBracketMatched(exp);
        auto answer_ok = std::get<0>(answer);
        auto answer_result = std::get<1>(answer);

        auto expectedAnswer = readAllText(filename + ".a");     
        auto expected_ok = (expectedAnswer == "Success");
        auto expected_result = 0;
        if(!expected_ok)
        {
            expected_result = atoi(expectedAnswer.c_str());
        }
        
        REQUIRE(answer_ok == expected_ok);
        REQUIRE(answer_result == expected_result);
    });
}

TEST_CASE("Bracket must work", "[Bracket]")
{
    runFileTestsCases();

    //REQUIRE(std::get<0>(IsBracketMatched("()")) == true);
    //REQUIRE(std::get<0>(IsBracketMatched("(())")) == true);
    //REQUIRE(std::get<0>(IsBracketMatched("()()")) == true);
    //REQUIRE(std::get<1>(IsBracketMatched("()(")) == 3);
    // REQUIRE(std::get<1>(IsBracketMatched("{")) == 1);
    // REQUIRE(std::get<1>(IsBracketMatched("}")) == 1);
}

#else

int main() {
    std::string text;
    getline(std::cin, text);
    auto answer = IsBracketMatched(text);
    auto answer_ok = std::get<0>(answer);
    auto answer_result = std::get<1>(answer);

    if(answer_ok)
    {
        std::cout << "Success" << std::endl;
    }
    else
    {
        std::cout << answer_result << std::endl;
    }

    return 0;
}

#endif