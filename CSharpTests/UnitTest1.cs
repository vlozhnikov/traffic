using NUnit.Framework;
using fTrafficCore;
using System.Diagnostics;

namespace CSharpTests
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
            //Assert.Pass();

            var a = new int[,]
            {
                {1, 0, 2},
                {3, 1, 0},
            };
            var A = Matrix.ofArray2D(a);
            var T = Matrix.transpose(A);
            Debug.Print(T.ToString());
        }
    }
}