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
class MyVector {
public:
    MyVector(): m_data(), m_capacity(0){}

    MyVector(const MyVector& other)
    {
        copy(other);
    }

    MyVector(MyVector&& other)
    {
        move(std::move(other));
    }

    MyVector& operator=(const MyVector& other)
    {
        copy(other);
        return *this;
    }

    MyVector& operator=(MyVector&& other)
    {
        move(std::move(other));
        return *this;
    }

    T& operator[](size_t i) const
    {
        assert(i < m_capacity);
        return m_data[i];
    }

    T* begin() const
    {
        return &m_data[0];
    }

    T* end() const
    {
        return &m_data[m_capacity];
    }

    void reserve(size_t newCap)
    {
        if (newCap > m_capacity)
        {
            std::unique_ptr<T[]> newData(new T[newCap]);
            if (m_data)
            {
                std::copy(begin(), end(), &newData[0]);
            }
            std::swap(m_data, newData);
            m_capacity = newCap;
        }
    }

    size_t capacity() const
    {
        return m_capacity;
    }

    void clear()
    {
        std::fill(begin(), end(), T());
    }

private:
    void move(MyVector&& other)
    {
        m_data.reset(other.m_data.release());
        m_capacity = other.m_capacity;
        other.m_capacity = 0;
    }

    void copy(const MyVector& other)
    {
        if (other.capacity() == 0)
        {
            m_data.reset();
            m_capacity = 0;
        }
        else
        {
            m_capacity = other.capacity();
            m_data.reset(new T[m_capacity]);
            std::copy(other.begin(), other.end(), begin());
        }
    }

    std::unique_ptr<T[]>    m_data;
    size_t                  m_capacity;
};

class BigNumber {
public:
    enum class Sign {Positive, Negative};

    MyVector<uint8_t>   m_data;
    size_t              m_digitCount;
    Sign                m_sign;

    BigNumber()
    {
        init(0);
    }

    BigNumber(const BigNumber& other)
    {
        copy(other);
    }

    BigNumber(BigNumber&& other)
    {
        move(std::move(other));
    }

    BigNumber(int n)
    {
        init(n);
    }

    void swap(BigNumber& other)
    {
        std::swap(m_digitCount, other.m_digitCount);
        std::swap(m_sign, other.m_sign);
        std::swap(m_data, other.m_data);
    }

    int64_t to_int64() const
    {
        assert(m_digitCount <= 18);
        int64_t retVal = 0;
        uint16_t i = 0;
        int64_t power10 = 1;
        while (i < m_digitCount)
        {
            retVal += retVal + m_data[i] * power10;
            power10 *= 10;
            i++;
        }
        return (m_sign == Sign::Positive) ? retVal : -retVal;
    }

    int32_t to_int32() const
    {
        assert(m_digitCount <= 9);
        return static_cast<int32_t>(to_int64());
    }

    std::tuple<BigNumber, BigNumber> split(uint16_t pivot) const
    {
        assert(pivot != 0);
        if (pivot >= m_digitCount)
            return std::make_tuple(BigNumber(),BigNumber(*this));

        pivot = m_digitCount - pivot; //number is stored in reverse order - begin with less semnificative digit

        BigNumber n1;
        BigNumber n2;

        n1.ensureCapacity(pivot);
        n1.m_digitCount = pivot;
        std::copy(m_data.begin() + m_digitCount - pivot, m_data.begin() + m_digitCount, n1.m_data.begin());

        n2.ensureCapacity(m_digitCount - pivot);
        n2.m_digitCount = m_digitCount - pivot;
        std::copy(m_data.begin(), m_data.begin() + m_digitCount - pivot, n2.m_data.begin());

        n1.normalize();
        n2.normalize();

        return std::make_tuple(std::move(n1),std::move(n2));
    }

    static BigNumber negate(const BigNumber& other)
    {
        BigNumber temp(other);
        temp.m_sign = (other.m_sign == Sign::Negative) ? Sign::Positive : Sign::Negative;
        return temp;
    }

    bool operator==(const BigNumber& other) const
    {
        return (m_sign == other.m_sign) &&
                (m_digitCount == other.m_digitCount) &&
                std::equal(m_data.begin(), m_data.begin() + m_digitCount, other.m_data.begin());
    }

    BigNumber& operator=(const BigNumber& other)
    {
        if (this != &other)
        {
            copy(other);
        }
        return *this;
    }

    BigNumber& operator=(BigNumber&& other)
    {
        assert(this != &other);

        move(std::move(other));

        return *this;
    }

    void normalize()
    {
        while (m_data[m_digitCount - 1] == 0 && m_digitCount > 1) //number should have at least one digit (that can be zero)
        {
            m_digitCount--;
        }
    }

    bool isnormalized()
    {
        return isZero() || ((m_digitCount > 0) && (m_data[m_digitCount - 1] != 0));
    }

    bool isZero() const
    {
        return (m_digitCount == 1) && (m_data[0] == 0);
    }

    bool operator <(const BigNumber& other) const
    {
        if (isZero() && other.isZero())
        {
            return false;
        }

        if ((m_sign == Sign::Negative) && (other.m_sign == Sign::Positive))
        {
            return true;
        }

        if ((other.m_sign == Sign::Negative) && (m_sign == Sign::Positive))
        {
            return false;
        }

        if (m_digitCount < other.m_digitCount)
        {
            return (m_sign == Sign::Negative) ? false : true;
        }
        else if (m_digitCount == other.m_digitCount)
        {
            uint16_t i = m_digitCount - 1;
            for(; (m_data[i] == m_data[i]) && (i > 0); i--);

            if (m_data[i] == other.m_data[i])
            {
                return false;
            }
            else if (m_data[i] < other.m_data[i])
            {
                return (m_sign == Sign::Negative) ? false : true;
            }
            else
            {
                return (m_sign == Sign::Positive) ? false : true;
            }
        }
        else
        {
            return (m_sign == Sign::Positive) ? false : true;
        }
    }

    bool operator <=(const BigNumber& other) const
    {
        return operator <(other) || operator==(other);
    }

    bool absless(const BigNumber& other) const
    {
        if (m_digitCount != other.m_digitCount)
        {
            return m_digitCount < other.m_digitCount;
        }

        uint16_t i = m_digitCount - 1;
        for(; (m_data[i] == other.m_data[i]) && (i > 0); i--);

        return (m_data[i] < other.m_data[i]);
    }

    bool abslessorequal(const BigNumber& other) const
    {
        if((m_digitCount == other.m_digitCount) &&
            std::equal(m_data.begin(), m_data.begin() + m_digitCount, other.m_data.begin()))
        {
            return true;
        }

        return absless(other);
    }

    void normalizedSubtraction(const BigNumber& subtrahend)
    {
        assert(subtrahend.abslessorequal(*this));

        u_int16_t digitCount1 = 0;
        int8_t carry = 0;
        while (digitCount1 < m_digitCount && digitCount1 < subtrahend.m_digitCount)
        {
            int8_t dif = m_data[digitCount1] - subtrahend.m_data[digitCount1] - carry;
            if (dif < 0)
            {
                dif += 10;
                carry = 1;
            } else
                carry = 0;
            m_data[digitCount1] = dif;
            digitCount1++;
        }
        while (digitCount1 < m_digitCount)
        {
            int8_t dif = m_data[digitCount1] - carry;
            if (dif < 0)
            {
                dif += 10;
                carry = 1;
            } else
                carry = 0;
            m_data[digitCount1] = dif;
            digitCount1++;
        }

        assert(carry == 0);

        normalize();
    }

    void normalizedAddition(const BigNumber& other)
    {
        u_int16_t digitCount1 = 0;
        u_int8_t carry = 0;
        while (digitCount1 < m_digitCount && digitCount1 < other.m_digitCount)
        {
            u_int8_t sum = m_data[digitCount1] + other.m_data[digitCount1] + carry;
            m_data[digitCount1] = sum % 10;
            carry = sum / 10;
            digitCount1++;
        }
        while (digitCount1 < m_digitCount)
        {
            u_int8_t sum = m_data[digitCount1] + carry;
            m_data[digitCount1] = sum % 10;
            carry = sum / 10;
            digitCount1++;
        }
        while (digitCount1 < other.m_digitCount)
        {
            ensureCapacity(other.m_digitCount);
            u_int8_t sum = other.m_data[digitCount1] + carry;
            m_data[digitCount1] = sum % 10;
            carry = sum / 10;
            digitCount1++;
        }
        m_digitCount = digitCount1;

        addCarry(m_digitCount, carry);

        assert(isnormalized());
    }

    BigNumber& operator-=(const BigNumber& other)
    {
        if (((m_sign == Sign::Negative) && (other.m_sign == Sign::Positive)) ||
            ((other.m_sign == Sign::Negative) && m_sign == (Sign::Positive)))
        {
            normalizedAddition(other);
        }
        else if ((m_sign == Sign::Negative) && (other.m_sign == Sign::Negative))
        {
            return operator+=(negate(other));
        }
        else //((m_sign == Sign::Positive) && (other.m_sign == Sign::Positive))
        {
            if (absless(other))
            {
                BigNumber subtrahend(std::move(*this));
                copy(other);
                normalizedSubtraction(subtrahend);
            }
            else
            {
                normalizedSubtraction(other);
            }
        }

        assert(isnormalized());

        return *this;
    }

    BigNumber& operator+=(const BigNumber& other)
    {
        if ((m_sign == Sign::Negative && other.m_sign == Sign::Positive))
        {
            BigNumber savedThis(std::move(*this));
            copy(other);
            return operator-=(negate(savedThis));
        }
        else if ((other.m_sign == Sign::Negative) && (m_sign == Sign::Positive))
        {
            return operator-=(negate(other));
        }

        normalizedAddition(other);

        assert(isnormalized());

        return *this;
    }

    void ensureCapacity(uint16_t newCapacity)
    {
        while (newCapacity > m_data.capacity())
            m_data.reserve(m_data.capacity() * 2);
    }

    void addCarry(uint32_t i, uint8_t carry)
    {
        if (carry > 0)
        {
            ensureCapacity(m_digitCount + 1);
            m_data[i] = carry;
            m_digitCount++;
        }

        assert(isnormalized());
    }

    BigNumber& mul10(uint16_t pos)
    {
        if (isZero())
        {
            return *this;
        }

        ensureCapacity(m_digitCount + pos);
        std::copy_backward(m_data.begin(), m_data.begin() + m_digitCount, m_data.begin() + m_digitCount + pos);
        for (uint16_t i = 0; i < pos; i++)
            m_data[i] = 0;
        m_digitCount += pos;

        assert(isnormalized());

        return *this;
    }

    BigNumber& mul(const BigNumber& other)
    {
        assert(m_digitCount == 1 || other.m_digitCount == 1);

        if (isZero())
        {
            return *this;
        }

        if (other.isZero())
        {
            copy(other);
            return *this;
        }

        BigNumber temp;
        if (other.m_digitCount > 1)
        {
            temp = std::move(*this);
            copy(other);
        }
        else
        {
            temp = other;
        }
        // *this has now the longest number and temp has 1 digit
        uint8_t carry = 0;
        uint16_t i = 0;

        while (i < m_digitCount)
        {
            uint8_t val = m_data[i] * temp.m_data[0] + carry;
            m_data[i] = val % 10;
            carry = val / 10;
            i++;
        }
        addCarry(i, carry);

        assert(isnormalized());

        return *this;
    }

    BigNumber& operator*=(const BigNumber& other) //karatsuba
    {
        if (m_digitCount == 1 || other.m_digitCount == 1)
        {
            return mul(other);
        }

        /* calculates the size of the numbers */
        uint16_t m = std::max(m_digitCount, other.m_digitCount);
        uint16_t m2 = m / 2;

        /* split the digit sequences about the middle */
        std::tuple<BigNumber, BigNumber> t1 = split(m2);
        std::tuple<BigNumber, BigNumber> t2 = other.split(m2);

        /* 3 calls made to numbers approximately half the size */
        BigNumber z0 = (std::get<1>(t1) * std::get<1>(t2));
        BigNumber z1 = ((std::get<1>(t1)) + std::get<0>(t1)) *=
                            ((std::get<1>(t2) + std::get<0>(t2)));
        BigNumber z2 = (std::get<0>(t1) * std::get<0>(t2));

        BigNumber z3 = (BigNumber(z2).mul10(2 * m2) += ((z1 -= z2) -= z0).mul10(m2)) += z0;

        move(std::move(z3));

        assert(isnormalized());

        return *this;
    }

    BigNumber operator*(const BigNumber& other)
    {
        return BigNumber(*this) *= other;
    }

    BigNumber operator-(const BigNumber& other)
    {
        return BigNumber(*this) -= other;
    }

    BigNumber operator+(const BigNumber& other)
    {
        return BigNumber(*this) += other;
    }

    void clear()
    {
        init(0);
    }

    void init(int n)
    {
        if (n >= 0)
            m_sign = Sign::Positive;
        else
            m_sign = Sign::Negative;
        n = std::abs(n);
        m_data.reserve(10);
        m_data.clear();

        if (n == 0)
        {
            m_digitCount = 1;
            m_data[0] = 0;
        } else
            m_digitCount = 0;

        while (n > 0)
        {
            m_data[m_digitCount] = (n % 10);
            n = n / 10;
            m_digitCount++;
        }
    }

    void copy(const BigNumber& n)
    {
        m_digitCount = n.m_digitCount;
        m_data.reserve(m_digitCount);
        if (m_digitCount > 0)
        {
            std::copy(n.m_data.begin(), n.m_data.begin() + m_digitCount, m_data.begin());
        }
        m_sign = n.m_sign;
    }

    void move(BigNumber&& other)
    {
        m_data = std::move(other.m_data);
        m_digitCount = other.m_digitCount;
        m_sign = other.m_sign;

        other.m_digitCount = 0;
    }
};

std::ostream& operator<<(std::ostream& os, const BigNumber& bn)
{
    os << '[' << static_cast<void*>(bn.m_data.begin()) << ':';
    if (bn.m_sign == BigNumber::Sign::Negative)
        os << '-';
    for (uint16_t i = bn.m_digitCount; i > 0; i--)
    {
        assert(bn.m_data[i - 1] < 10);
        os << char('0' + bn.m_data[i - 1]);
    }
    os << ':' << bn.m_digitCount << ']';
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

    auto res = BigNumber(16).abslessorequal(BigNumber(32));
    std::cout << "res:" << res << std::endl;

    std::cout << "test equal: " << (BigNumber(1073741824) == BigNumber(1024) * BigNumber(1024) * BigNumber(1024)) << std::endl;

    BigNumber b1 = BigNumber(1073741824); //2^30
    std::cout << b1 << std::endl;

    for (int i = 0; i < 2; i++)
    {
        std::cout << (b1 *= BigNumber(1073741824));
        std::cout << std::endl;
    }

    std::cout << (b1 *= BigNumber(1024));
    std::cout << std::endl;

    BigNumber b2(1);
    for (int i = 0; i < 100; i++)
    {
        b2 *= b1;
    }
    std::cout << b2;
    std::cout << std::endl;

    BigNumber b11(1);
    for (int i = 0; i < 10000; i++)
    {
        b11 += b11;
    }
    std::cout << b11;
    std::cout << std::endl;

    std::cout << "test 2^10000: " << (b2 == b11) << std::endl;

    return 0;
}
