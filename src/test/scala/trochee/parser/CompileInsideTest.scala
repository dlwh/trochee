package trochee.parser

import org.scalatest.FunSuite
import java.io.File
import com.nativelibs4java.opencl.JavaCL

/**
 *
 * @author dlwh
 */
class CompileInsideTest extends FunSuite {
  test("unaries") {
    val header = """
#define PARSE_CELL float*
#define CHART_SIZE 100

#define NUM_GRAMMARS 1
#define TRIANGULAR_INDEX(begin, end) ((end) * ((end)+1)/2 + begin)
#define NUM_RULES 1000

                   |typedef struct {
                   |  float rules[NUM_RULES][NUM_GRAMMARS];
                   |} rule_cell;
                 """.stripMargin
    val unaries = NullGrammar.codegen.mkKernel(NullGrammar.insideUnaries)
    val text = header + unaries

    val context = TestContext.context
    val p = context.createProgram(text)
    p.build()
  }

    test("binaries") {
    val header = """
#define PARSE_CELL float*
#define CHART_SIZE 100
#define NUM_GRAMMARS 1
#define TRIANGULAR_INDEX(begin, end) ((end) * ((end)+1)/2 + begin)
#define NUM_RULES 1000

                   |typedef struct {
                   |  float rules[NUM_RULES][NUM_GRAMMARS];
                   |} rule_cell;
                 """.stripMargin
    val binaries = NullGrammar.codegen.mkKernel(NullGrammar.insideTermBinaries)
    val text = header + binaries

    val context = TestContext.context
    val p = context.createProgram(text)
    p.build()


  }

}

object TestContext {
  implicit lazy val context = JavaCL.createBestContext()
}
