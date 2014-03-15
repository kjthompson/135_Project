// This program will find the overlap between neighboring lines of input 
// in a file.

import java.util.*;
import java.io.*;

// i need a map to use as 'finished' data set
public class Project {
  private static Scanner lineScanner;

  // post: returns total number of overlapping lines and index  of lines that
  //       have overlap with the proceeding line
  public static void main(String[] args) throws FileNotFoundException {
    Scanner file = new Scanner(new File("patterns.txt"));
    ArrayList<Integer> indexList = new ArrayList<Integer>();

    // initialize arrays and counters
    double[] p1 = new double[11];
    double[] p2 = new double[11];
    int overlapTotal = 0;
    int index = 1;

    // find overlap between a line and its predecessor
    while (file.hasNextLine()) {

      Scanner line = new Scanner(file.nextLine());
      int i = 0;
      while (line.hasNextDouble()) {
        p2[i] = line.nextDouble();
        i++;
      }
      int overlap = overlapCount(p1, p2);
      if (overlap == 1)
        indexList.add(index - 1);

      overlapTotal += overlap;
      // System.out.println(Arrays.toString(p1));
      // System.out.println(Arrays.toString(p2));
      p1 = p2.clone();
      index++;
    }

    // print results
    System.out.println(overlapTotal);
    System.out.println(indexList.toString());

  }


  // Knuth-Morris-Pratt Algorithm
  // mostly taken from stackoverflow
  //
  //
  // pre: a string s2 to be checked for overlap with with string s1
  // post: number of overlapped characters 
  public static int overlapCount(double[] p1, double[] p2) {

    int[] T = ComputeBackTrackTable(p2); 

    int m = 0;
    int i = 0;
    while (m + i < p1.length) {
        if (p2[i] == p1[m + i]) {
            i += 1;
        } else {
            m += i - T[i];
            if (i > 0) i = T[i];
        }
    }
    boolean overlapLogical = i > 0;
    int overlap = 0;
    if (overlapLogical)
      overlap++;

    // returns 1 if there was overlap, 0 otherwise
    return overlap; 
  }

  // no idea what this does
  public static int[] ComputeBackTrackTable(double[] p) {
    int[] T = new int[p.length];
    int cnd = 0;
    T[0] = -1;
    T[1] = 0;
    int pos = 2;
    while (pos < p.length) {
        if (p[pos - 1] == p[cnd]) {
            T[pos] = cnd + 1;
            pos += 1;
            cnd += 1;
        } else if (cnd > 0) {
            cnd = T[cnd];
        } else {
            T[pos] = 0;
            pos += 1;
        }
    }

    return T;
  }

}
