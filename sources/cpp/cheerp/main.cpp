#include <string>

#include <cheerp/client.h>
#include <cheerp/clientlib.h>

// We need to extend the client namespace to declare our
// custom JavaScript function
namespace client
{
    // The name should be the same as the JavaScript one
    // The parameters needs to be a const client::String reference
    // so that implicit conversion from const char* is supported
    void changeTitle(const String& str);
}

using namespace client;

void webMain()
{
    auto titleElement = document.getElementById("pagetitle");
    auto oldText = titleElement->get_textContent();
    changeTitle(oldText + "Literal C++ string");
}