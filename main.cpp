#include <iostream>
#include <array>
#include <vector>
#include <math.h>
#include <tuple>
#include <cassert>
#include <algorithm>
#include <memory>

#define MAX_DIGITS 4000
class BigNumber;
std::ostream& operator<<(std::ostream& os, const BigNumber& bn);

template <typename T>
class myVector {
public:
    myVector(): data(), _capacity(0){}

    myVector(const myVector& other) {
        copy(other);
    }
    myVector(myVector&& other) {
        move(std::move(other));
    }

    myVector& operator=(const myVector& other)
    {
        copy(other);
        return *this;
    }
    myVector& operator=(myVector&& other)
    {
        move(std::move(other));
        return *this;
    }

    T& operator[](uint32_t i) const
    {
        return data[i];
    }

    T* begin() const
    {
        return &data[0];
    }

    T* release()
    {
        return data.release();
    }

    void reserve(uint32_t newCap)
    {
        if (newCap > _capacity)
        {
            std::unique_ptr<T[]> newData(new T[newCap]);
            std::copy(begin(), begin() + _capacity, &newData[0]);
            std::swap(data, newData);
            _capacity = newCap;
        }
    }
    uint32_t capacity() const {return _capacity;}
    void clear()
    {
        std::fill_n(begin(), capacity(), T());
    }

private:
    void move(myVector&& other)
    {
        data.reset(other.release());
        _capacity = other.capacity();
    }
    void copy(const myVector& other)
    {
        _capacity = other.capacity();
        data.reset(new T[_capacity]);
        std::copy(other.begin(), other.begin() + _capacity, begin());
    }
    std::unique_ptr<T[]> data;
    uint32_t _capacity;
};

class BigNumber {
public:
    enum class Sign {Positive, Negative};
    myVector<uint8_t> data;
    uint16_t digitCount;
    Sign sign;
    BigNumber() {init(0);}
    BigNumber(const BigNumber& other){copy(other);}
    BigNumber(BigNumber&& other):data(std::move(other.data))
    {
        digitCount = other.digitCount;
        sign = other.sign;
    }
    BigNumber(int n) {init(n);}

    void swap(BigNumber& other){
        std::swap(digitCount, other.digitCount);
        Sign s1 = sign;
        sign = other.sign;
        other.sign = s1;
        std::swap(data, other.data);
    }
    int64_t to_int64() const
    {
        assert(digitCount <= 18);
        int64_t retVal = 0;
        uint16_t i = 0;
        int64_t power10 = 1;
        while (i < digitCount)
        {
            retVal += retVal + data[i] * power10;
            power10 *= 10;
            i++;
        }
        return (sign == Sign::Positive)?retVal:-retVal;
    }

    int32_t to_int32() const
    {
        assert(digitCount <= 9);
        return static_cast<int32_t>(to_int64());
    }

    std::tuple<BigNumber, BigNumber> split(uint16_t pivot) const
    {
        assert(pivot != 0);
        if (pivot >= digitCount)
            return std::make_tuple(BigNumber(),BigNumber(*this));
        pivot = digitCount - pivot; //because the number is stored in reverse order - begin with less semnificative digit
        BigNumber n1;
        BigNumber n2;
        uint16_t i = 0;
        uint16_t j = 0;
        bool n2Valid = false;
        if (pivot < digitCount)
        {
            i = digitCount - pivot;
            n2Valid = true;
        } else
            i = 0;
        n1.ensureCapacity(pivot);
        while (i < digitCount)
        {
            n1.data[j] = data[i];
            i++;j++;
        }
        n1.digitCount = j;

        if (n2Valid)
        {
            i = 0;
            n2.ensureCapacity(digitCount - pivot);
            while (i < digitCount - pivot)
            {
                n2.data[i] = data[i];
                i++;
            }
            n2.digitCount = i;
        }
        return std::make_tuple(std::move(n1),std::move(n2));
    }

    static BigNumber negate(const BigNumber& other)
    {
        BigNumber temp(other);
        temp.sign = (other.sign == Sign::Negative) ? Sign::Positive : Sign::Negative;
        return temp;
    }

    BigNumber& operator=(const BigNumber& other)
    {
        copy(other);
        return *this;
    }

    BigNumber& operator=(const BigNumber&& other)
    {
        data = std::move(other.data);
        digitCount = other.digitCount;
        sign = other.sign;
        return *this;
    }

    void normalize()
    {
        while (data[digitCount - 1] == 0 && digitCount > 1)
        {
            digitCount--;
        }
    }

    BigNumber& operator-=(const BigNumber& other)
    {
//        std::cout << "-this: " << *this << std::endl;
//        std::cout << "-other: " << other << std::endl;
        if ((sign == Sign::Negative && other.sign == Sign::Positive) || (other.sign == Sign::Negative && sign == Sign::Positive))
            return *this += negate(other);

        BigNumber temp;
        if (digitCount < other.digitCount)
        {
            temp = *this;
            copy(other);
            sign = (other.sign == Sign::Negative) ? Sign::Positive : Sign::Negative;
        }
        else if (digitCount == other.digitCount)
        {
            if (other.data[digitCount - 1] > data[digitCount - 1])
            {
                temp = *this;
                copy(other);
                sign = (other.sign == Sign::Negative) ? Sign::Positive : Sign::Negative;
            } else
            {
                temp = other;
            }
        } else
            temp = other;

        u_int16_t digitCount1 = 0;
        int8_t carry = 0;
        while (digitCount1 < digitCount && digitCount1 < temp.digitCount)
        {
            int8_t dif = data[digitCount1] - temp.data[digitCount1] - carry;
            if (dif < 0)
            {
                dif += 10;
                carry = 1;
            } else
                carry = 0;
            data[digitCount1] = dif;
            digitCount1++;
        }
        while (digitCount1 < digitCount)
        {
            int8_t dif = data[digitCount1] - carry;
            if (dif < 0)
            {
                dif += 10;
                carry = 1;
            } else
                carry = 0;
            data[digitCount1] = dif;
            digitCount1++;
        }
        assert(carry == 0);
        normalize();
//        std::cout << " -this: " << *this << std::endl;
        return *this;
    }

    BigNumber& operator+=(const BigNumber& other)
    {
//        std::cout << "+this: " << *this << std::endl;
//        std::cout << "+other: " << other << std::endl;
        if ((sign == Sign::Negative && other.sign == Sign::Positive) || (other.sign == Sign::Negative && sign == Sign::Positive))
            return *this -= negate(other);

        u_int16_t digitCount1 = 0;
        u_int8_t carry = 0;
        while (digitCount1 < digitCount && digitCount1 < other.digitCount)
        {
            u_int8_t sum = data[digitCount1] + other.data[digitCount1] + carry;
            data[digitCount1] = sum % 10;
            carry = sum / 10;
            digitCount1++;
        }
        while (digitCount1 < digitCount)
        {
            u_int8_t sum = data[digitCount1] + carry;
            data[digitCount1] = sum % 10;
            carry = sum / 10;
            digitCount1++;
        }
        while (digitCount1 < other.digitCount)
        {
            ensureCapacity(other.digitCount);
            u_int8_t sum = other.data[digitCount1] + carry;
            data[digitCount1] = sum % 10;
            carry = sum / 10;
            digitCount1++;
        }
        digitCount = digitCount1;
        addCarry(digitCount, carry);
//        std::cout << " +this: " << *this << std::endl;
        return *this;
    }

    void ensureCapacity(uint16_t newCapacity)
    {
        while (newCapacity > data.capacity())
            data.reserve(data.capacity() * 2);
    }

    void addCarry(uint32_t i, uint8_t carry)
    {
        if (carry > 0)
        {
            ensureCapacity(digitCount + 1);
            data[i] = carry;
            digitCount++;
        }
    }

    BigNumber& mul10(uint16_t pos)
    {
        ensureCapacity(digitCount + pos);
        std::copy_backward(data.begin(), data.begin() + digitCount, data.begin() + digitCount + pos);
        for (uint16_t i = 0; i < pos; i++)
            data[i] = 0;
        digitCount += pos;
        normalize();
        return *this;
    }

    BigNumber& mul(const BigNumber& other)
    {
        assert(digitCount == 1 || other.digitCount == 1);
        BigNumber temp;
        if ((digitCount == 1) && (other.digitCount > 1))
        {
            temp = *this;
            copy(other);
        } else
            temp = other;
        // temp has for sure 1 digit
        uint8_t carry = 0;
        uint16_t i = 0;

        while (i < digitCount)
        {
            uint8_t val = data[i] * temp.data[0] + carry;
            data[i] = val % 10;
            carry = val / 10;
            i++;
        }
        addCarry(i, carry);
        normalize();
//        std::cout << " mul this: " << *this << std::endl;
        return *this;
    }

    BigNumber& operator*=(const BigNumber& other) //karatsuba
    {
//        std::cout << "*this: " << *this << std::endl;
//        std::cout << "*other: " << other << std::endl;
        if (digitCount == 1 || other.digitCount == 1)
        {
            return mul(other);
        }
        /* calculates the size of the numbers */
        uint16_t m = std::max(digitCount, other.digitCount);
        uint16_t m2 = m / 2;

        /* split the digit sequences about the middle */
        std::tuple<BigNumber, BigNumber> t1 = split(m2);
        std::tuple<BigNumber, BigNumber> t2 = other.split(m2);
        /* 3 calls made to numbers approximately half the size */
        BigNumber z0 = (BigNumber(std::get<1>(t1)) *= std::get<1>(t2));
        BigNumber z1 = ((BigNumber(std::get<1>(t1)) += std::get<0>(t1)) *=
                            (BigNumber(std::get<1>(t2)) += std::get<0>(t2)));
        BigNumber z2 = BigNumber(std::get<0>(t1)) *= std::get<0>(t2);
        BigNumber z3 = (BigNumber(z2).mul10(2 * m2) += ((BigNumber(z1) -= z2) -= z0).mul10(m2)) += z0;
        copy(z3);
        normalize();
//        std::cout << " *this: " << *this << std::endl;
        return *this;
    }

    void clear() {init(0);}
    void init(int n){
        if (n >= 0)
            sign = Sign::Positive;
        else
            sign = Sign::Negative;
        n = std::abs(n);
        data.reserve(10);
        data.clear();

        if (n == 0)
        {
            digitCount = 1;
            data[0] = 0;
        } else
            digitCount = 0;

        while (n > 0)
        {
            data[digitCount] = (n % 10);
            n = n / 10;
            digitCount++;
        }
    }

    void copy(const BigNumber& n){
        digitCount = n.digitCount;
        data.reserve(digitCount);
        std::copy(n.data.begin(), n.data.begin() + digitCount, data.begin());
        sign = n.sign;
    }
};

std::ostream& operator<<(std::ostream& os, const BigNumber& bn)
{
    os << '[' << &bn << ':';
    if (bn.sign == BigNumber::Sign::Negative)
        os << '-';
    for (uint16_t i = bn.digitCount; i > 0; i--)
        os << char('0' + bn.data[i - 1]);
    os << ':' << bn.digitCount << ']';
    return os;
}

int main(int argc, char *argv[])
{
//    std::cout << BigNumber(9).mul(BigNumber(99)) << std::endl;
//    std::tuple<BigNumber, BigNumber> t = BigNumber(1234567).split(5);
//    std::cout << std::get<0>(t) << std::endl;
//    std::cout << std::get<1>(t) << std::endl;

//    t = BigNumber(120).split(1);
//    std::cout << std::get<0>(t) << std::endl;
//    std::cout << std::get<1>(t) << std::endl;
//    BigNumber(1) - BigNumber(2);
//    std::cout << std::endl;

//    BigNumber(1) + BigNumber(-2) + BigNumber(5);
//    std::cout << std::endl;

//    BigNumber(999) -= BigNumber(1000);
//    std::cout << std::endl;

//    BigNumber a = (BigNumber(3) *= BigNumber(9));
//    std::cout << "a: " << a << std::endl;

//    std::cout << BigNumber(2).mul10(2);
//    std::cout << std::endl;
//    std::cout << (BigNumber(20).mul10(1) += BigNumber(2).mul10(2));
//    std::cout << std::endl;
//    std::cout << BigNumber(3).mul(-202);
//    std::cout << std::endl;

//    std::cout << (BigNumber(312).mul10(10) -= BigNumber(312));
//    std::cout << std::endl;


    BigNumber b1 = BigNumber(1073741824); //2^30
    std::cout << b1;
    std::cout << std::endl;
    for (int i = 0; i < 2; i++)
    {
        std::cout << (b1 *= BigNumber(1073741824));
        std::cout << std::endl;
    }

    std::cout << (b1 *= BigNumber(1024));
    std::cout << std::endl;

    BigNumber b2 = b1;
    for (int i = 0; i < 99; i++)
    {
        b1 *= b2;
    }
    std::cout << b1;
    std::cout << std::endl;

    return 0;
}
