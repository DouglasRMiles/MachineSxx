#ifndef EXCEPTIONHELPER
#define EXCEPTIONHELPER

#include <exception>

class NumberFormatException : public std::exception
{
private:
    std::string msg;

public:
    NumberFormatException(const std::string& message = "") : msg(message)
    {
    }

    const char * what() const throw()
    {
        return msg.c_str();
    }
};

class IOException : public std::exception
{
private:
    std::string msg;

public:
    IOException(const std::string& message = "") : msg(message)
    {
    }

    const char * what() const throw()
    {
        return msg.c_str();
    }
};

class NullPointerException : public std::exception
{
private:
    std::string msg;

public:
    NullPointerException(const std::string& message = "") : msg(message)
    {
    }

    const char * what() const throw()
    {
        return msg.c_str();
    }
};

class IllegalStateException : public std::exception
{
private:
    std::string msg;

public:
    IllegalStateException(const std::string& message = "") : msg(message)
    {
    }

    const char * what() const throw()
    {
        return msg.c_str();
    }
};


#endif	//#ifndef EXCEPTIONHELPER
