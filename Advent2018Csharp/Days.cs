using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Claims;
using System.Text.RegularExpressions;
using Microsoft.VisualStudio.TestPlatform.Utilities;
using NUnit.Framework;

namespace Tests
{
    public class Tests
    {
        [SetUp]
        public void Setup()
        {
        }

        [Test]
        public void Day1Part2()
        {
            var file = "../../../../../../code/advent/input1.txt";
            var lines = System.IO.File.ReadLines(file);
            var numbers = lines.Select(int.Parse).ToList();
            var set = new HashSet<int>();

            int current = 0;
            do
            {
                current = AddStuff(set, numbers, current);
            } while (true);
        }

        private int AddStuff(HashSet<int> set, IList<int> numbers, int initial)
        {
            int current = initial;
            foreach (var number in numbers)
            {
                current = current + number;
                if (set.Contains(current))
                {
                    throw new System.Exception(current.ToString());
                }
                else
                {
                    set.Add(current);
                }
            }
            return current;
        }

        [Test]
        public void Day2Part1()
        {
            var file = "../../../../../../code/advent/input2.txt";
            var dicts = System.IO.File.ReadLines(file)
                .Select(str => str.ToCharArray(0, str.Length))
                .Select(FillDictionary);
            var twinCount = 0;
            var tripletCount = 0;
            foreach (var dict in dicts)
            {
                twinCount =  dict.ContainsValue(2) ? twinCount + 1 : twinCount;
                tripletCount = dict.ContainsValue(3) ? tripletCount + 1 : tripletCount;
            }

            var checksum = twinCount * tripletCount;
            Console.WriteLine($"Checksum: {checksum.ToString()}");
            Assert.Pass();
        }

        private Dictionary<char, int> FillDictionary(char[] line)
        {
            var dict = new Dictionary<char, int>();
            foreach (var charc in line)
            {
                if (dict.ContainsKey(charc))
                {
                    dict[charc] = dict[charc] + 1;
                }
                else
                {
                    dict[charc] = 1;
                }
            }

            return dict;
        }
        
        [Test]
        public void Day2Part2()
        {
            var file = "../../../../../../code/advent/input2.txt";
            var lines = System.IO.File.ReadLines(file)
                .Select(str => str.ToCharArray(0, str.Length))
                .ToList();
            foreach (var outer in lines)
            {
                foreach (var inner in lines)
                {
                    var matched = MatchArrays(outer, inner);
                    if (matched.Count() == 25)
                    {
                        throw new Exception(new string(matched.ToArray()));
                    }
                }
            }
        }

        private IEnumerable<char> MatchArrays(char[] a, char[] b)
        {
            var zip = a.Zip(b, (x,y) => (x,y));
            foreach (var tuple in zip)
            {
                if (tuple.Item1 == tuple.Item2)
                {
                    yield return tuple.Item1;
                }
            }
        }

        private struct Claim
        {
            public int Id;
            public int X;
            public int Y;
            public int Width;
            public int Height;
        }
        
        [Test]
        public void Day3Part1()
        {
            var file = "../../../../../../code/advent/input3.txt";
            var lines = System.IO.File.ReadLines(file)
                .ToList();

            const int rows = 1000;
            const int cols = 1000;
            int[] data = new int[rows*cols];

            void IncrementClaim(Claim c)
            {
                void IncrementPosition(int x, int y)
                {
                    var index = rows * x + y;
                    ++data[index];
                }

                foreach (var row in Enumerable.Range(c.Y, c.Height))
                {
                    foreach (var col in Enumerable.Range(c.X, c.Width))
                    {
                        IncrementPosition(row, col);
                    }
                }
                
            }
            foreach (var line in lines)
            {
                var claim = GetClaim(line);
                IncrementClaim(claim);
            }

            int sum = 0;
            foreach (var i in data)
            {
                if (i >= 2)
                {
                    ++sum;
                }
            }

            Console.WriteLine(sum.ToString());
        }
        
        [Test]
        public void Day3Part2()
        {
            var file = "../../../../../../code/advent/input3.txt";
            var lines = System.IO.File.ReadLines(file)
                .ToList();

            const int rows = 1000;
            const int cols = 1000;
            int[] data = new int[rows*cols];

            void IncrementClaim(Claim c)
            {
                void IncrementPosition(int x, int y)
                {
                    var index = rows * x + y;
                    ++data[index];
                }

                foreach (var row in Enumerable.Range(c.Y, c.Height))
                {
                    foreach (var col in Enumerable.Range(c.X, c.Width))
                    {
                        IncrementPosition(row, col);
                    }
                }
                
            }
            var claims = lines.Select(GetClaim).ToList();
            foreach (var claim in claims)
            {
                IncrementClaim(claim);
            }

            bool VerifyClaim(Claim c)
            {
                foreach (var row in Enumerable.Range(c.Y, c.Height))
                {
                    foreach (var col in Enumerable.Range(c.X, c.Width))
                    {
                        if (data[rows * row + col] != 1)
                        {
                            return false;
                        }
                    }
                }

                return true;
            }
            foreach (var claim in claims)
            {
                if (VerifyClaim(claim))
                {
                    throw new Exception($"Claim: {claim.Id}");
                }
            }

        }

        Regex ClaimRegex = new Regex("#(?<Id>\\d+) @ (?<X>\\d+),(?<Y>\\d+): (?<Width>\\d+)x(?<Height>\\d+)");
        private Claim GetClaim(string line)
        {
            var result = ClaimRegex.Match(line);
            return new Claim()
            {
                Id = int.Parse(result.Groups["Id"].ToString()),
                X = int.Parse(result.Groups["X"].ToString()),
                Y = int.Parse(result.Groups["Y"].ToString()),
                Width = int.Parse(result.Groups["Width"].ToString()),
                Height = int.Parse(result.Groups["Height"].ToString()),
            };
        }
    }
}