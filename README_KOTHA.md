# BangLang - বাংলা প্রোগ্রামিং ল্যাঙ্গুয়েজ

এটি একটি **কম্প্যাক্ট** বাংলা প্রোগ্রামিং ল্যাঙ্গুয়েজ কম্পাইলার যা মাত্র **একটি ফাইলে** সম্পূর্ণ কম্পাইলার রয়েছে।

## 🚀 দ্রুত শুরু

### কোড কীভাবে রান করতে হয়

#### ১. কম্পাইল করা
```bash
# C++ কম্পাইলার দিয়ে কম্পাইল করুন
g++ -std=c++17 -Wall -Wextra -O2 -o banglang_compact banglang.cpp

# অথবা Makefile ব্যবহার করুন
make -f Makefile
```

#### ২. প্রোগ্রাম রান করা
```bash
# ডিফল্ট: input.txt → output.txt
./banglang_compact

# কাস্টম ইনপুট/আউটপুট ফাইল
./banglang_compact -i my_input.txt -o my_output.txt

# C কোড জেনারেট
./banglang_compact -c -o output.c

# সাহায্য দেখুন
./banglang_compact -h
```

#### ৩. সহজ স্ক্রিপ্ট ব্যবহার
```bash
# compile.sh স্ক্রিপ্ট ব্যবহার
./compile.sh
```

## 📁 প্রজেক্ট স্ট্রাকচার

```
banglang.cpp           # 🎯 মূল কম্পাইলার (একটি ফাইলে সব)
Makefile               # 🔧 কম্পাইল করার জন্য
compile.sh             # 🚀 সহজ ব্যবহারের স্ক্রিপ্ট
input.txt              # 📝 ইনপুট ফাইল (আপনার কোড)
output.txt             # 📄 আউটপুট ফাইল (প্রোগ্রাম রেজাল্ট)
output_c.txt           # 📄 C কোড আউটপুট
sample.bang            # 📝 সিম্পল টেস্ট প্রোগ্রাম
README.md              # 📖 বিস্তারিত ব্যবহার নির্দেশিকা
```

## 🌟 ফিচার

### বাংলা কীওয়ার্ড
- `পূর্ণসংখ্যা` - ভেরিয়েবল ডিক্লেয়ারেশন (int)
- `যদি` - কন্ডিশনাল স্টেটমেন্ট (if)
- `নইলে` - else ব্লক
- `দেখাও` - প্রিন্ট স্টেটমেন্ট (print)

### বাংলা সংখ্যা
- `০-৯` - বাংলা সংখ্যা (স্বয়ংক্রিয়ভাবে ইংরেজি সংখ্যায় রূপান্তর)

### অপারেটর
- `+`, `-`, `*`, `/` - গাণিতিক অপারেশন
- `>`, `<` - তুলনা
- `=` - অ্যাসাইনমেন্ট

## 📋 উদাহরণ

### ১. বেসিক ভেরিয়েবল এবং প্রিন্ট
```banglang
পূর্ণসংখ্যা x = ৫;
দেখাও(x);
```

**আউটপুট:**
```
Variable 'x' declared with value: 5
Output: 5
```

### ২. গাণিতিক অপারেশন
```banglang
পূর্ণসংখ্যা x = ১০;
পূর্ণসংখ্যা y = ৩;
দেখাও(x + y);
দেখাও(x - y);
দেখাও(x * y);
দেখাও(x / y);
```

**আউটপুট:**
```
Variable 'x' declared with value: 10
Variable 'y' declared with value: 3
Output: 13
Output: 7
Output: 30
Output: 3
```

### ৩. তুলনা অপারেশন
```banglang
পূর্ণসংখ্যা x = ১০;
পূর্ণসংখ্যা y = ৩;
দেখাও(x > y);
দেখাও(x < y);
```

**আউটপুট:**
```
Variable 'x' declared with value: 10
Variable 'y' declared with value: 3
Output: 1
Output: 0
```

### ৪. কন্ডিশনাল স্টেটমেন্ট
```banglang
পূর্ণসংখ্যা x = ১০;
যদি (x > ৫) {
    দেখাও(x);
} নইলে {
    দেখাও(০);
}
```

**আউটপুট:**
```
Variable 'x' declared with value: 10
Condition is true, executing if block...
Output: 10
```

### ৫. একাধিক প্যারামিটার প্রিন্ট
```banglang
পূর্ণসংখ্যা x = ৫;
পূর্ণসংখ্যা y = ৬;
দেখাও(x, y);
```

**আউটপুট:**
```
Variable 'x' declared with value: 5
Variable 'y' declared with value: 6
Output: 5 6
```

## 🔧 কমান্ড লাইন অপশন

```bash
./banglang_compact [options]

Options:
  -i <file>     ইনপুট ফাইল (ডিফল্ট: input.txt)
  -o <file>     আউটপুট ফাইল (ডিফল্ট: output.txt)
  -c           C কোড জেনারেট করুন
  -h, --help   সাহায্য দেখুন
```

## 📄 আউটপুট ফাইল

### output.txt (ডিফল্ট আউটপুট)
```
// BangLang Direct Execution Output
// ================================

Program Output:
==============
1
0
13
```

### output_c.txt (C কোড আউটপুট)
```c
// Generated C Code from BangLang
// ==============================

#include <stdio.h>
#include <stdlib.h>

int main() {
    int x = 10;
    int y = 3;
    printf("%d\n", 1);
    printf("%d\n", 0);
    printf("%d\n", 13);
    return 0;
}
```

## 🛠️ ইনস্টলেশন

### প্রয়োজনীয়তা
- C++17 কম্পাইলার (g++ 7.0+)
- Make (optional)

### ইনস্টলেশন স্টেপ
```bash
# ১. প্রজেক্ট ক্লোন করুন
git clone <repository-url>
cd BangLang

# ২. কম্পাইল করুন
g++ -std=c++17 -Wall -Wextra -O2 -o banglang_compact banglang.cpp

# ৩. টেস্ট করুন
./banglang_compact
```

## 🧪 টেস্টিং

### বেসিক টেস্ট
```bash
# টেস্ট ইনপুট তৈরি করুন
echo "পূর্ণসংখ্যা x = ৫; দেখাও(x);" > input.txt

# রান করুন
./banglang_compact

# আউটপুট চেক করুন
cat output.txt
```

### C কোড জেনারেশন টেস্ট
```bash
# C কোড জেনারেট করুন
./banglang_compact -c -o test.c

# C কোড কম্পাইল করুন
gcc test.c -o test_program

# রান করুন
./test_program
```

## 📝 সিনট্যাক্স রুলস

### ভেরিয়েবল ডিক্লেয়ারেশন
```banglang
পূর্ণসংখ্যা <variable_name> = <value>;
```

### প্রিন্ট স্টেটমেন্ট
```banglang
দেখাও(<expression>);
দেখাও(<expression1>, <expression2>);  // একাধিক প্যারামিটার
```

### কন্ডিশনাল স্টেটমেন্ট
```banglang
যদি (<condition>) {
    <statements>
} নইলে {
    <statements>
}
```

### এক্সপ্রেশন
```banglang
<expression> + <expression>    // যোগ
<expression> - <expression>    // বিয়োগ
<expression> * <expression>    // গুণ
<expression> / <expression>    // ভাগ
<expression> > <expression>    // বড়
<expression> < <expression>    // ছোট
```

## 🐛 সমস্যা সমাধান

### কমন এরর
1. **কম্পাইল এরর**: C++17 সাপোর্ট চেক করুন
2. **ফাইল নট ফাউন্ড**: input.txt ফাইল আছে কিনা চেক করুন
3. **সিনট্যাক্স এরর**: সেমিকোলন (;) ভুলে যাবেন না

### ডিবাগিং
```bash
# ভারবোস আউটপুট
./banglang_compact -v

# C কোড জেনারেট করে চেক করুন
./banglang_compact -c -o debug.c
cat debug.c
```

## 🤝 কন্ট্রিবিউশন

প্রজেক্টে অবদান রাখতে চাইলে:
1. ইস্যু রিপোর্ট করুন
2. ফিচার রিকোয়েস্ট করুন
3. পুল রিকোয়েস্ট পাঠান

## 📄 লাইসেন্স

এই প্রজেক্ট MIT লাইসেন্সের অধীনে প্রকাশিত।

## 📞 যোগাযোগ

প্রশ্ন বা সমস্যা থাকলে ইস্যু খুলুন অথবা যোগাযোগ করুন।

---

**বাংলা প্রোগ্রামিং ল্যাঙ্গুয়েজ দিয়ে প্রোগ্রামিং করুন! 🇧🇩** 