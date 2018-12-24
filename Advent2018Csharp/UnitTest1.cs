using System.Collections.Generic;
using System.Linq;
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
        public void Test1()
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
    }
}