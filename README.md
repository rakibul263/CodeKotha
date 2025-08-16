# 🇧🇩 কোডকথা (CodeKotha) - বাংলা প্রোগ্রামিং ল্যাঙ্গুয়েজ

<div align="center">

![CodeKotha Logo](https://img.shields.io/badge/CodeKotha-বাংলা%20প্রোগ্রামিং-green?style=for-the-badge&logo=code)
![Version](https://img.shields.io/badge/version-1.0.0-blue?style=for-the-badge)
![Language](https://img.shields.io/badge/language-C%2B%2B17-orange?style=for-the-badge)
![License](https://img.shields.io/badge/license-MIT-red?style=for-the-badge)

**বাংলা ভাষায় প্রোগ্রামিং করুন! 🚀**

*একটি সম্পূর্ণ বাংলা প্রোগ্রামিং ল্যাঙ্গুয়েজ কম্পাইলার*

</div>

---

## 📖 বিবরণ

**কোডকথা** একটি বিপ্লবী বাংলা প্রোগ্রামিং ল্যাঙ্গুয়েজ যা সম্পূর্ণ বাংলা সিনট্যাক্স ব্যবহার করে। এটি একটি কম্প্যাক্ট C++ কম্পাইলার যা বাংলা কোডকে সরাসরি এক্সিকিউট করে অথবা C কোড জেনারেট করে।

### ✨ বিশেষত্ব
- 🎯 **সম্পূর্ণ বাংলা সিনট্যাক্স** - কোন ইংরেজি কীওয়ার্ডের প্রয়োজন নেই
- ⚡ **দ্রুত কম্পাইলেশন** - একটি ফাইলেই সম্পূর্ণ কম্পাইলার
- 🔄 **দ্বৈত আউটপুট** - সরাসরি এক্সিকিউশন অথবা C কোড জেনারেশন
- 🌐 **ক্রস-প্ল্যাটফর্ম** - Windows, macOS, Linux সাপোর্ট
- 📚 **সহজ শেখা** - পরিচিত বাংলা শব্দাবলী ব্যবহার

---

## 🚀 দ্রুত শুরু

### ⚙️ প্রয়োজনীয়তা
- **C++17** কম্পাইলার (GCC 7.0+ বা Clang 5.0+)
- **Make** (ঐচ্ছিক)

### 📥 ইনস্টলেশন

#### 1️⃣ প্রজেক্ট ক্লোন করুন
```bash
git clone https://github.com/yourusername/CodeKotha.git
cd CodeKotha
```

#### 2️⃣ কম্পাইল করুন
```bash
# সহজ উপায়
./compile_kotha.sh

# অথবা ম্যানুয়ালি
g++ -std=c++17 -Wall -Wextra -O2 -o codekotha codekotha.cpp

# অথবা Makefile ব্যবহার করুন
make -f Makefile_kotha
```

#### 3️⃣ রান করুন
```bash
# ডিফল্ট ইনপুট ফাইল দিয়ে রান করুন
./codekotha

# কাস্টম ইনপুট ফাইল ব্যবহার করুন
./codekotha -i my_program.kotha -o my_output.txt
```

---

## 🏗️ প্রজেক্ট স্ট্রাকচার

```
CodeKotha/
├── 📄 codekotha.cpp          # মূল কম্পাইলার সোর্স কোড
├── 🔧 Makefile_kotha         # বিল্ড কনফিগারেশন
├── 🚀 compile_kotha.sh       # সহজ কম্পাইল স্ক্রিপ্ট
├── 📝 input.txt              # ডিফল্ট ইনপুট ফাইল
├── 📄 output.txt             # এক্সিকিউশন আউটপুট
├── 🖥️ output_c_kotha.txt     # জেনারেটেড C কোড
├── 📚 example_kotha.txt      # উদাহরণ প্রোগ্রাম
├── 📖 README.md              # এই ফাইল
└── 🔐 .gitignore            # গিট কনফিগ
```

---

## 🌟 ভাষার ফিচার

### 🔤 বাংলা কীওয়ার্ড

| বাংলা কীওয়ার্ড | ইংরেজি সমতুল্য | বিবরণ |
|----------------|----------------|-------|
| `পূর্ণসংখ্যা` | `int` | ইন্টিজার ভেরিয়েবল ডিক্লেয়ার করে |
| `দেখাও` | `print` | মান প্রিন্ট করে |
| `যদি` | `if` | শর্তাধীন স্টেটমেন্ট |
| `নইলে` | `else` | বিকল্প শর্ত |

### 🔢 বাংলা সংখ্যা সাপোর্ট

কোডকথা বাংলা সংখ্যা (`০-৯`) সম্পূর্ণভাবে সাপোর্ট করে:

```banglang
পূর্ণসংখ্যা x = ১০;  // 10
পূর্ণসংখ্যা y = ৫;   // 5
```

### ⚙️ অপারেটর

| অপারেটর | কাজ | উদাহরণ |
|----------|-----|---------|
| `+` | যোগ | `x + y` |
| `-` | বিয়োগ | `x - y` |
| `*` | গুণ | `x * y` |
| `/` | ভাগ | `x / y` |
| `>` | বড় | `x > y` |
| `<` | ছোট | `x < y` |
| `=` | মান নির্ধারণ | `x = ১০` |

---

## 💡 কোড উদাহরণ

### 🎯 বেসিক ভেরিয়েবল এবং প্রিন্ট

```banglang
পূর্ণসংখ্যা x = ৫;
দেখাও(x);
```

**আউটপুট:**
```
Variable 'x' declared with value: 5
Output: 5
```

### 🔢 গাণিতিক অপারেশন

```banglang
পূর্ণসংখ্যা x = ১০;
পূর্ণসংখ্যা y = ৩;
দেখাও(x + y);  // যোগ
দেখাও(x - y);  // বিয়োগ
দেখাও(x * y);  // গুণ
দেখাও(x / y);  // ভাগ
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

### 🔍 তুলনা অপারেশন

```banglang
পূর্ণসংখ্যা x = ১০;
পূর্ণসংখ্যা y = ৫;
দেখাও(x > y);  // ১ (true)
দেখাও(x < y);  // ০ (false)
```

### 🔀 কন্ডিশনাল স্টেটমেন্ট

```banglang
পূর্ণসংখ্যা x = ১৫;
যদি (x > ১০) {
    দেখাও("x বড় সংখ্যা");
    দেখাও(x);
} নইলে {
    দেখাও("x ছোট সংখ্যা");
    দেখাও(০);
}
```

### 📊 একাধিক প্যারামিটার

```banglang
পূর্ণসংখ্যা x = ৫;
পূর্ণসংখ্যা y = ৭;
দেখাও(x, y);  // একসাথে দুটি মান প্রিন্ট
```

---

## 🛠️ ব্যবহার নির্দেশিকা

### 📥 ইনপুট ফাইল প্রস্তুতি

1. আপনার বাংলা কোড `input.txt` ফাইলে লিখুন
2. অথবা কাস্টম ফাইল ব্যবহার করুন

```bash
echo "পূর্ণসংখ্যা x = ১০; দেখাও(x);" > my_program.kotha
```

### 🎯 কম্পাইলার চালানো

#### ডিফল্ট মোড (input.txt → output.txt)
```bash
./codekotha
```

#### কাস্টম ইনপুট/আউটপুট
```bash
./codekotha -i my_program.kotha -o result.txt
```

#### C কোড জেনারেশন
```bash
./codekotha -c -o generated_code.c
```

#### সাহায্য দেখুন
```bash
./codekotha -h
```

---

## 📄 আউটপুট ফরম্যাট

### 🖥️ সরাসরি এক্সিকিউশন আউটপুট

```
=== BangLang Program Execution ===
Executing program statements...
==================================
Variable 'x' declared with value: 10
Variable 'y' declared with value: 5
Output: 15
Output: 5
Output: 50
==================================
Program execution completed!
```

### 💾 আউটপুট ফাইল (output.txt)

```
// BangLang Direct Execution Output
// ================================

Program Output:
==============
15
5
50
2
1
```

### 🖥️ জেনারেটেড C কোড

```c
// Generated C Code from BangLang
// ==============================

#include <stdio.h>
#include <stdlib.h>

int main() {
    int x = 10;
    int y = 5;
    printf("%d\n", 15);
    printf("%d\n", 5);
    printf("%d\n", 50);
    return 0;
}
```

---

## 🔧 কমান্ড লাইন অপশন

```bash
./codekotha [অপশনসমূহ]

অপশনসমূহ:
  -i <ফাইল>      ইনপুট ফাইল নির্ধারণ (ডিফল্ট: input.txt)
  -o <ফাইল>      আউটপুট ফাইল নির্ধারণ (ডিফল্ট: output.txt)
  -c              C কোড জেনারেট করুন
  -h, --help      সাহায্য প্রদর্শন করুন
  -v, --version   ভার্শন তথ্য দেখুন
```

---

## 🧪 টেস্টিং

### 🎯 বেসিক টেস্ট

```bash
# টেস্ট প্রোগ্রাম তৈরি
echo "পূর্ণসংখ্যা x = ৫; দেখাও(x);" > test_input.txt

# রান করুন
./codekotha -i test_input.txt -o test_output.txt

# ফলাফল দেখুন
cat test_output.txt
```

### 🔄 C কোড টেস্ট

```bash
# C কোড জেনারেট করুন
./codekotha -c -i input.txt -o generated.c

# C কোড কম্পাইল করুন
gcc generated.c -o test_program

# চালান
./test_program
```

### 📊 উদাহরণ প্রোগ্রাম রান

```bash
# উদাহরণ ফাইল রান করুন
./codekotha -i example_kotha.txt
```

---

## 📝 সিনট্যাক্স গাইড

### 🔤 ভেরিয়েবল ডিক্লেয়ারেশন

```banglang
পূর্ণসংখ্যা <ভেরিয়েবল_নাম> = <মান>;
```

**উদাহরণ:**
```banglang
পূর্ণসংখ্যা age = ২৫;
পূর্ণসংখ্যা count = ০;
```

### 🖨️ প্রিন্ট স্টেটমেন্ট

```banglang
দেখাও(<এক্সপ্রেশন>);
দেখাও(<এক্সপ্রেশন১>, <এক্সপ্রেশন২>);
```

**উদাহরণ:**
```banglang
দেখাও(age);
দেখাও(x, y);
দেখাও(x + y);
```

### 🔀 কন্ডিশনাল স্টেটমেন্ট

```banglang
যদি (<শর্ত>) {
    <স্টেটমেন্টসমূহ>
} নইলে {
    <স্টেটমেন্টসমূহ>
}
```

### 🧮 এক্সপ্রেশন সিনট্যাক্স

```banglang
<এক্সপ্রেশন> + <এক্সপ্রেশন>    // যোগ
<এক্সপ্রেশন> - <এক্সপ্রেশন>    // বিয়োগ
<এক্সপ্রেশন> * <এক্সপ্রেশন>    // গুণ
<এক্সপ্রেশন> / <এক্সপ্রেশন>    // ভাগ
<এক্সপ্রেশন> > <এক্সপ্রেশন>    // বড়
<এক্সপ্রেশন> < <এক্সপ্রেশন>    // ছোট
```

---

## 🐛 সমস্যা সমাধান

### ❌ সাধারণ ত্রুটিসমূহ

1. **কম্পাইল এরর**
   ```
   error: 'banglang.cpp' not found
   ```
   **সমাধান:** ফাইলের নাম `codekotha.cpp` ব্যবহার করুন

2. **ফাইল নট ফাউন্ড**
   ```
   Error: Cannot open input file
   ```
   **সমাধান:** `input.txt` ফাইল তৈরি করুন অথবা সঠিক পাথ দিন

3. **সিনট্যাক্স এরর**
   ```
   Syntax error: Expected ';'
   ```
   **সমাধান:** প্রতিটি স্টেটমেন্টের শেষে সেমিকোলন (`;`) দিন

### 🔍 ডিবাগিং টিপস

```bash
# ভারবোস আউটপুট
./codekotha -v

# C কোড চেক করুন
./codekotha -c -o debug.c
cat debug.c

# স্টেপ বাই স্টেপ টেস্ট
echo "পূর্ণসংখ্যা x = ৫;" > simple_test.txt
./codekotha -i simple_test.txt
```

---

## 🚀 উন্নত ফিচার

### 🔄 ব্যাচ প্রসেসিং

```bash
# একাধিক ফাইল প্রসেস করুন
for file in *.kotha; do
    ./codekotha -i "$file" -o "${file%.kotha}.out"
done
```

### 📊 পারফরম্যান্স পরিমাপ

```bash
# টাইম মাপুন
time ./codekotha -i large_program.kotha

# মেমরি ব্যবহার চেক করুন
valgrind ./codekotha
```

---

## 🤝 কন্ট্রিবিউশন

আমরা আপনার অবদানকে স্বাগত জানাই! 🎉

### 📋 কীভাবে অবদান রাখবেন

1. **Fork** করুন এই রিপোজিটরি
2. **Feature branch** তৈরি করুন: `git checkout -b feature/amazing-feature`
3. **Commit** করুন: `git commit -m 'Add amazing feature'`
4. **Push** করুন: `git push origin feature/amazing-feature`
5. **Pull Request** খুলুন

### 🎯 অবদানের ক্ষেত্রসমূহ

- 🐛 **বাগ ফিক্স** - ত্রুটি খুঁজে সমাধান করুন
- ✨ **নতুন ফিচার** - ভাষায় নতুন বৈশিষ্ট্য যোগ করুন
- 📚 **ডকুমেন্টেশন** - README ও কোড কমেন্ট উন্নত করুন
- 🧪 **টেস্ট কেস** - আরো টেস্ট যোগ করুন
- 🎨 **UI/UX** - ব্যবহারকারী অভিজ্ঞতা উন্নত করুন

### 📝 কোড স্ট্যান্ডার্ড

- C++17 স্ট্যান্ডার্ড ব্যবহার করুন
- কোডে বাংলা কমেন্ট যোগ করুন
- ফাংশন ও ভেরিয়েবলের সুস্পষ্ট নাম দিন

---

## 📈 রোডম্যাপ

### 🎯 আসন্ন ফিচারসমূহ

- [ ] 🔄 **লুপ স্টেটমেন্ট** (`পুনরাবৃত্তি`, `যতক্ষণ`)
- [ ] 🔢 **ফ্লোট নাম্বার সাপোর্ট** (`দশমিক`)
- [ ] 📝 **স্ট্রিং ভেরিয়েবল** (`লেখা`)
- [ ] 📋 **অ্যারে সাপোর্ট** (`তালিকা`)
- [ ] 🔧 **ফাংশন ডেফিনিশন** (`কাজ`)
- [ ] 📁 **ফাইল অপারেশন**
- [ ] 🌐 **ইন্টারেক্টিভ মোড**

### 🔮 ভবিষ্যৎ পরিকল্পনা

- 🖥️ **GUI এডিটর** - ভিজুয়াল কোড এডিটর
- 🌍 **ওয়েব ভার্শন** - ব্রাউজারে চালানো
- 📱 **মোবাইল অ্যাপ** - Android/iOS সাপোর্ট
- 🤖 **AI সহায়তা** - কোড অটো-কমপ্লিশন

---

## 📊 পরিসংখ্যান

<div align="center">

| মেট্রিক | মান |
|---------|-----|
| **মোট কোড লাইন** | 705+ |
| **সাপোর্টেড কীওয়ার্ড** | 4+ |
| **বাংলা সংখ্যা** | ০-৯ |
| **অপারেটর** | 7+ |
| **কম্পাইল টাইম** | < 1 সেকেন্ড |

</div>

---

## 🏆 কৃতজ্ঞতা

বিশেষ ধন্যবাদ:

- 🇧🇩 **বাংলাদেশী ডেভেলপার কমিউনিটি** - অনুপ্রেরণার জন্য
- 💻 **ওপেন সোর্স কন্ট্রিবিউটরগণ** - সাহায্যের জন্য
- 🎓 **শিক্ষার্থীরা** - ফিডব্যাকের জন্য

---

## 🔗 উপযোগী লিংকসমূহ

- 📚 [C++ Reference](https://cppreference.com/)
- 🎓 [বাংলা প্রোগ্রামিং টিউটোরিয়াল](#)
- 💬 [কমিউনিটি ফোরাম](#)
- 🐛 [ইস্যু ট্র্যাকার](https://github.com/yourusername/CodeKotha/issues)

---

## 📄 লাইসেন্স

এই প্রজেক্ট **MIT License** এর অধীনে প্রকাশিত।

```
MIT License

Copyright (c) 2024 CodeKotha Project

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
```

---

## 📞 যোগাযোগ ও সাহায্য

### 💬 সাহায্য পেতে

- 🐛 **বাগ রিপোর্ট:** [Issues](https://github.com/yourusername/CodeKotha/issues)
- 💡 **ফিচার রিকোয়েস্ট:** [Feature Request](https://github.com/yourusername/CodeKotha/issues/new)
- 📧 **ইমেইল:** codekotha@example.com
- 💬 **ডিসকর্ড:** [CodeKotha Community](#)

### 🌟 সোশ্যাল

- 🐦 **Twitter:** [@CodeKotha](#)
- 📘 **Facebook:** [CodeKotha Page](#)
- 📸 **Instagram:** [@codekotha](#)

---

<div align="center">

## 🎉 কোডকথা দিয়ে বাংলায় প্রোগ্রামিং শুরু করুন!

**"প্রোগ্রামিং এখন আর কঠিন নয় - বাংলায় কোড লিখুন!" 🇧🇩**

---

### ⭐ স্টার দিন | 🔄 শেয়ার করুন | 🤝 অবদান রাখুন

[![GitHub stars](https://img.shields.io/github/stars/yourusername/CodeKotha?style=social)](https://github.com/yourusername/CodeKotha/stargazers)
[![GitHub forks](https://img.shields.io/github/forks/yourusername/CodeKotha?style=social)](https://github.com/yourusername/CodeKotha/network)

**Made with ❤️ in Bangladesh 🇧🇩**

</div>
