#define GLM_FORCE_RADIANS

#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <fstream>
#include <vector>
#include <functional>
#include <random>

#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/ext.hpp>

using namespace glm;

GLuint loadShaders(std::string vertex_file_path,
                   std::string fragment_file_path);

#define checkNoGlError() __checkNoGlError(__FILE__, __LINE__)

void __checkNoGlError(std::string file, int line) {
  GLenum glError = glGetError();

  if (glError != GL_NO_ERROR) {
    fprintf(stderr, "GL ERROR in file %s @ line %d : %s\n", file.c_str(), line,
            gluErrorString(glError));
    exit(-1);
  }
}

class ViewManager {
 public:
  ViewManager(GLFWwindow* window) : window_(window) {}

  void scrollCallback(double xoffset, double yoffset) {
    fov_ += 3.14 / 36 * yoffset;
  }

  void cursorPosCallback(double x, double y) {
    int width, height;
    glfwGetWindowSize(window_, &width, &height);

    glfwSetCursorPos(window_, width / 2, height / 2);

    double dx = (width / 2.0 - x);
    double dy = (height / 2.0 - y);

    horizontal_angle_ += mouse_speed_ /** delta_time */ * dx;
    vertical_angle_ += mouse_speed_ /** delta_time */ * dy;

    // printf("%f %f\n", horizontal_angle_, vertical_angle_);
  }

  glm::mat4 getMVPMatrix() {
    glm::vec3 direction(cos(vertical_angle_) * sin(horizontal_angle_),
                        sin(vertical_angle_),
                        cos(vertical_angle_) * cos(horizontal_angle_));

    glm::mat4 projection = glm::perspective(fov_, 4.0f / 3.0f, 0.1f, 100.0f);

    float r = 5;

    glm::vec3 eye_pos =
        glm::vec3(r * sin(vertical_angle_) * cos(horizontal_angle_),
                  r * sin(vertical_angle_) * sin(horizontal_angle_),
                  r * cos(vertical_angle_));

    glm::vec3 side = glm::cross(glm::vec3(0, 0, 1), eye_pos);
    if (glm::length(side) == 0) {
      // looking straight from the top.
      side = glm::cross(glm::vec3(1, 0, 0), eye_pos);
    }

    glm::vec3 up = glm::vec3(cos(horizontal_angle_) * cos(vertical_angle_),
                             sin(horizontal_angle_) * cos(vertical_angle_),
                             -sin(vertical_angle_));

    glm::mat4 view = glm::lookAt(eye_pos, glm::vec3(0, 0, 0), up);
    glm::mat4 mvp = projection * view;

    return mvp;
  }

 private:
  GLFWwindow* window_;

  glm::vec3 camera_position_ = glm::vec3(0, 0, 5);
  float horizontal_angle_ = 3.14f;
  float vertical_angle_ = 0.0f;
  float fov_ = 3.14 / 4;
  float speed_ = 3.0f;  // 3 units / second
  float mouse_speed_ = 0.005f;
};

class Window {
 public:
  Window(GLFWwindow* window) : window_(window), view_manager_(window) {
    glfwSetWindowUserPointer(window_, this);

    glfwSetScrollCallback(window, &Window::scrollCallback);
    glfwSetCursorPosCallback(window, &Window::cursorPosCallback);
  }

  static Window* getInstance(GLFWwindow* window) {
    return (Window*)glfwGetWindowUserPointer(window);
  }

  ViewManager& view_manager() { return view_manager_; }

 private:
  static void scrollCallback(GLFWwindow* window, double xoffset,
                             double yoffset) {
    getInstance(window)->view_manager_.scrollCallback(xoffset, yoffset);
  }

  static void cursorPosCallback(GLFWwindow* window, double x, double y) {
    getInstance(window)->view_manager_.cursorPosCallback(x, y);
  }

  GLFWwindow* window_;
  ViewManager view_manager_;
};

void addTriangle(glm::vec3 p1, glm::vec3 p2, glm::vec3 p3,
                 std::vector<GLfloat>* triangles) {
  triangles->push_back(p1[0]);
  triangles->push_back(p1[1]);
  triangles->push_back(p1[2]);
  triangles->push_back(p2[0]);
  triangles->push_back(p2[1]);
  triangles->push_back(p2[2]);
  triangles->push_back(p3[0]);
  triangles->push_back(p3[1]);
  triangles->push_back(p3[2]);
}

static std::random_device RANDOM;
static std::uniform_real_distribution<> ZERO_ONE(0, 1);

static int vertexPosition_modelspace = 0;
static int vertexNormal_modelspace = 1;
static int vertexColor = 2;

class Stopwatch {
 public:
  Stopwatch(std::string name) : name_(name), start_(clock()) { }

  ~Stopwatch() { printf("%s took %'ld ms\n", name_.c_str(), (clock() - start_) * 1000 / CLOCKS_PER_SEC); }

 private:
  const std::string name_;
  const clock_t start_;
};

class Buffer {
 public:
  Buffer() {
    glGenBuffers(1, &id_);
    checkNoGlError();
  }

  ~Buffer() {
    glDeleteBuffers(1, &id_);
    checkNoGlError();
  }

  void bind(GLenum target) {
    glBindBuffer(target, id_);
    checkNoGlError();
  }

 private:
  GLuint id_;
};

class Cube;

class GLScene {
 public:
  GLScene() {
    glGenVertexArrays(1, &vertexArray_);
    checkNoGlError();
  }

  void addTriangle(const glm::vec3& p1,
                   const glm::vec3& p2,
                   const glm::vec3& p3,
                   const glm::vec3& c1,
                   const glm::vec3& c2,
                   const glm::vec3& c3) {
    push_vector(p1, &triangles_);
    push_vector(p2, &triangles_);
    push_vector(p3, &triangles_);

    push_vector(c1, &colors_);
    push_vector(c2, &colors_);
    push_vector(c3, &colors_);

    glm::vec3 n = glm::normalize(glm::cross(p2 - p1, p3 - p1));

    push_vector(n, &normals_);
    push_vector(n, &normals_);
    push_vector(n, &normals_);
  }

  void fillBuffers() {
    glBindVertexArray(vertexArray_);

    vertexBuffer_.bind(GL_ARRAY_BUFFER);
    glBufferData(GL_ARRAY_BUFFER, triangles_.size() * sizeof(GLfloat),
                 &triangles_[0], GL_STATIC_DRAW);
    checkNoGlError();

    colorBuffer_.bind(GL_ARRAY_BUFFER);
    glBufferData(GL_ARRAY_BUFFER, colors_.size() * sizeof(GLfloat), &colors_[0],
                 GL_STATIC_DRAW);
    checkNoGlError();

    normalsBuffer_.bind(GL_ARRAY_BUFFER);
    glBufferData(GL_ARRAY_BUFFER, normals_.size() * sizeof(GLfloat),
                 &normals_[0], GL_STATIC_DRAW);
    checkNoGlError();

    glEnableVertexAttribArray(vertexPosition_modelspace);
    glEnableVertexAttribArray(vertexColor);
    glEnableVertexAttribArray(vertexNormal_modelspace);
  }

  void draw() {
    // draw
    int arrayIndex = 0;

    glEnableVertexAttribArray(arrayIndex);
    checkNoGlError();

    glBindVertexArray(vertexArray_);
    vertexBuffer_.bind(GL_ARRAY_BUFFER);
    checkNoGlError();

    glVertexAttribPointer(vertexPosition_modelspace, 3, GL_FLOAT, GL_FALSE, 0,
                          (void*)0);
    checkNoGlError();

    colorBuffer_.bind(GL_ARRAY_BUFFER);
    glVertexAttribPointer(vertexColor, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);
    checkNoGlError();

    normalsBuffer_.bind(GL_ARRAY_BUFFER);
    glVertexAttribPointer(vertexNormal_modelspace, 3, GL_FLOAT, GL_FALSE, 0,
                          (void*)0);
    checkNoGlError();

    glDrawArrays(GL_TRIANGLES, 0, triangles_.size() / 3);
    checkNoGlError();

    glDisableVertexAttribArray(arrayIndex);
    checkNoGlError();
  }

  void setCubes(const std::vector<Cube> cubes);

 private:
  void push_vector(const glm::vec3& p, std::vector<GLfloat>* vector) {
    if (vector->capacity() <= vector->size()) {
      printf("ERROR: vector overfill %zu vs %zu\n", vector->capacity(), vector->size());
    }
    vector->push_back(p[0]);
    vector->push_back(p[1]);
    vector->push_back(p[2]);
  }

  std::vector<GLfloat> triangles_;
  std::vector<GLfloat> colors_;
  std::vector<GLfloat> normals_;
  Buffer vertexBuffer_;
  GLuint vertexArray_;
  Buffer colorBuffer_;
  Buffer normalsBuffer_;
};
inline float squared(float v) { return v * v; }

glm::vec3 randomColor() {
  return glm::vec3(ZERO_ONE(RANDOM), ZERO_ONE(RANDOM), ZERO_ONE(RANDOM));
}

class Cube {
 public:
  Cube(glm::vec3 p1, glm::vec3 p2, glm::vec3 color)
      : p1_(glm::vec3(std::min(p1[0], p2[0]), std::min(p1[1], p2[1]),
                      std::min(p1[2], p2[2]))),
        p2_(glm::vec3(std::max(p1[0], p2[0]), std::max(p1[1], p2[1]),
                      std::max(p1[2], p2[2]))),
        c_(color) {}

  void addVertices(GLScene* scene) const {
    glm::vec3 p1 = p1_;
    glm::vec3 p2 = p2_;
    glm::vec3 p3(p2[0], p1[1], p1[2]);
    glm::vec3 p4(p1[0], p1[1], p2[2]);
    glm::vec3 p5(p2[0], p1[1], p2[2]);
    glm::vec3 p6(p1[0], p2[1], p1[2]);
    glm::vec3 p7(p2[0], p2[1], p1[2]);
    glm::vec3 p8(p1[0], p2[1], p2[2]);

    scene->addTriangle(p1, p5, p3, c_, c_, c_);
    scene->addTriangle(p1, p4, p5, c_, c_, c_);
    scene->addTriangle(p4, p2, p5, c_, c_, c_);
    scene->addTriangle(p4, p8, p2, c_, c_, c_);

    scene->addTriangle(p3, p5, p2, c_, c_, c_);
    scene->addTriangle(p3, p2, p7, c_, c_, c_);

    scene->addTriangle(p8, p4, p1, c_, c_, c_);
    scene->addTriangle(p6, p8, p1, c_, c_, c_);

    scene->addTriangle(p6, p2, p8, c_, c_, c_);
    scene->addTriangle(p6, p7, p2, c_, c_, c_);

    scene->addTriangle(p1, p7, p6, c_, c_, c_);
    scene->addTriangle(p1, p3, p7, c_, c_, c_);
  }

  std::vector<Cube> subdivs() {
    std::vector<Cube> result;

    glm::vec3 m = (p1_ + p2_) * 0.5f;

    result.push_back(Cube(glm::vec3(p1_[0], p1_[1], p1_[2]),
                          glm::vec3(m[0], m[1], m[2]), c_));
    result.push_back(Cube(glm::vec3(p1_[0], p1_[1], m[2]),
                          glm::vec3(m[0], m[1], p2_[2]), c_));
    result.push_back(Cube(glm::vec3(p1_[0], m[1], p1_[2]),
                          glm::vec3(m[0], p2_[1], m[2]), c_));
    result.push_back(Cube(glm::vec3(p1_[0], m[1], m[2]),
                          glm::vec3(m[0], p2_[1], p2_[2]), c_));

    result.push_back(Cube(glm::vec3(m[0], p1_[1], p1_[2]),
                          glm::vec3(p2_[0], m[1], m[2]), c_));
    result.push_back(Cube(glm::vec3(m[0], p1_[1], m[2]),
                          glm::vec3(p2_[0], m[1], p2_[2]), c_));
    result.push_back(Cube(glm::vec3(m[0], m[1], p1_[2]),
                          glm::vec3(p2_[0], p2_[1], m[2]), c_));
    result.push_back(Cube(glm::vec3(m[0], m[1], m[2]),
                          glm::vec3(p2_[0], p2_[1], p2_[2]), c_));

    return result;
  }

 private:
  friend class Sphere;

  glm::vec3 p1_;
  glm::vec3 p2_;
  glm::vec3 c_;
};

void GLScene::setCubes(const std::vector<Cube> cubes) {
  printf("%zu cubes\n", cubes.size());
  Stopwatch stopwatch("setCubes");

  triangles_.clear();
  colors_.clear();
  normals_.clear();

  triangles_.reserve(cubes.size() * 12 * 9);
  normals_.reserve(cubes.size() * 12 * 9);
  colors_.reserve(cubes.size() * 12 * 9);

  for (auto& cube : cubes) {
    cube.addVertices(this);
  }
}



class SceneObject {
 public:
  virtual ~SceneObject() {}

  virtual bool intersectsCube(Cube& cube) const = 0;
  virtual bool containsCube(Cube& cube) const = 0;
};

class Sphere : public SceneObject {
 public:
  Sphere(glm::vec3 c, float r) : c_(c), r_(r) {}

  virtual bool intersectsCube(Cube& cube) const {
    float r2 = r_ * r_;
    float dmin = 0;

    for (int i = 0; i < 3; i++) {
      if (c_[i] < cube.p1_[i])
        dmin += squared(c_[i] - cube.p1_[i]);
      else if (c_[i] > cube.p2_[i])
        dmin += squared(c_[i] - cube.p2_[i]);
    }

    return dmin <= r2;
  }

  virtual bool containsCube(Cube& cube) const {
    float r2 = r_ * r_;

    for (int i = 0; i < 2; i++) {
      for (int j = 0; j < 2; j++) {
        for (int k = 0; k < 2; k++) {
          float dist2 = squared((i == 0 ? cube.p1_[0] : cube.p2_[0]) - c_[0]) +
                        squared((j == 0 ? cube.p1_[1] : cube.p2_[1]) - c_[1]) +
                        squared((k == 0 ? cube.p1_[2] : cube.p2_[2]) - c_[2]);

          if (dist2 > r2) {
            return false;
          }
        }
      }
    }

    return true;
  }

 private:
  const glm::vec3 c_;
  const float r_;
};

class Union : public SceneObject {
 public:
  Union(std::vector<SceneObject*> objects) : objects_(objects) {}

  virtual bool intersectsCube(Cube& cube) const {
    for (auto object : objects_) {
      if (object->intersectsCube(cube)) {
        return true;
      }
    }

    return false;
  }

  virtual bool containsCube(Cube& cube) const {
    for (auto object : objects_) {
      if (object->containsCube(cube)) {
        return true;
      }
    }

    return false;
  }

 private:
  const std::vector<SceneObject*> objects_;
};

class Intersection : public SceneObject {
 public:
  Intersection(std::vector<SceneObject*> objects) : objects_(objects) {}

  virtual bool intersectsCube(Cube& cube) const {
    for (auto object : objects_) {
      if (!object->intersectsCube(cube)) {
        return false;
      }
    }

    return true;
  }

  virtual bool containsCube(Cube& cube) const {
    for (auto object : objects_) {
      if (!object->containsCube(cube)) {
        return false;
      }
    }

    return true;
  }

 private:
  const std::vector<SceneObject*> objects_;
};

std::vector<Cube> performSubdivision(std::vector<Cube>& cubes,
                                     const SceneObject& sceneObject) {
  Stopwatch stopwatch("subdiv");
  std::vector<Cube> result;

  glm::vec3 c(0, 0, 0);

  for (auto cube : cubes) {
    if (sceneObject.containsCube(cube)) {
      result.push_back(cube);
    } else {
      for (auto sub_cube : cube.subdivs()) {
        if (sceneObject.intersectsCube(sub_cube)) {
          result.push_back(sub_cube);
        }
      }
    }
  }

  return result;
}

int main() {
  setlocale(LC_NUMERIC, "");

  if (!glfwInit()) {
    fprintf(stderr, "Failed to initialize GLFW\n");
    return -1;
  }

  glfwWindowHint(GLFW_SAMPLES, 4);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 4);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  checkNoGlError();

  GLFWwindow* window;
  window = glfwCreateWindow(1024, 768, "Tutorial 01", NULL, NULL);
  if (window == NULL) {
    fprintf(stderr,
            "Failed to open GLFW window. If you have an Intel GPU, they are "
            "not 3.3 compatible. Try the 2.1 version of the tutorials.\n");
    glfwTerminate();
    return -1;
  }
  checkNoGlError();

  glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);

  Window window_wrapper(window);

  glfwMakeContextCurrent(window);  // Initialize GLEW
  checkNoGlError();

  glewExperimental = true;  // Needed in core profile
  if (glewInit() != GLEW_OK) {
    fprintf(stderr, "Failed to initialize GLEW\n");
    return -1;
  }
  glGetError();  // https://www.opengl.org/wiki/OpenGL_Loading_Library
  checkNoGlError();

  // Ensure we can capture the escape key being pressed below
  glfwSetInputMode(window, GLFW_STICKY_KEYS, GL_TRUE);

  checkNoGlError();

  GLuint program_id = loadShaders("test.vertexshader", "test.fragmentshader");
  checkNoGlError();

  glUseProgram(program_id);
  checkNoGlError();

  std::vector<Cube> subdivs = {Cube(glm::vec3(-10, -10, -10),
                                    glm::vec3(10, 10, 10), glm::vec3(1, 0, 0))};

  GLScene glScene;
  glScene.setCubes(subdivs);
  glScene.fillBuffers();

  glEnable(GL_DEPTH_TEST);
  glFrontFace(GL_CW);
  glEnable(GL_CULL_FACE);
  glCullFace(GL_BACK);
  glDepthFunc(GL_LESS);

  //  glEnable(GL_BLEND);
  //  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  bool doSubdivs = true;

  Union scene = Union({
      new Intersection({
          new Sphere(glm::vec3(0, 0, 0), 0.75),
          new Sphere(glm::vec3(0, 1, 0), 0.75),
      }),
      new Intersection({
          new Sphere(glm::vec3(1, 1, 0), 1), new Sphere(glm::vec3(1, 0, 0), 1),
      }),
  });

  do {
    if (glfwGetKey(window, GLFW_KEY_SPACE) == GLFW_PRESS && doSubdivs) {
      doSubdivs = false;
      subdivs = performSubdivision(subdivs, scene);

      glScene.setCubes(subdivs);

      {
        Stopwatch addVertices("fillBuffers");
        glScene.fillBuffers();
      }
    } else {
      doSubdivs = glfwGetKey(window, GLFW_KEY_SPACE) == GLFW_RELEASE;
    }

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    checkNoGlError();

    glm::mat4 mvp = window_wrapper.view_manager().getMVPMatrix();
    glm::mat3 m_3x3_inv_transp = glm::transpose(glm::inverse(glm::mat3(mvp)));

    GLuint matrix_id = glGetUniformLocation(program_id, "MVP");
    glUniformMatrix4fv(matrix_id, 1, GL_FALSE, &mvp[0][0]);
    checkNoGlError();

    GLuint m_3x3_inv_transp_id =
        glGetUniformLocation(program_id, "m_3x3_inv_transp");
    glUniformMatrix3fv(m_3x3_inv_transp_id, 1, GL_FALSE,
                       &m_3x3_inv_transp[0][0]);
    checkNoGlError();

    glScene.draw();

    // Swap buffers
    glfwSwapBuffers(window);
    glfwPollEvents();
    checkNoGlError();

  } while (glfwGetKey(window, GLFW_KEY_ESCAPE) != GLFW_PRESS &&
           glfwWindowShouldClose(window) == 0);
}

std::string loadFileContent(std::string file_path) {
  std::ifstream stream(file_path.c_str(),
                       std::ios::in | std::ios::binary | std::ios::ate);

  if (stream.is_open()) {
    std::ifstream::pos_type fileSize = stream.tellg();
    stream.seekg(0, std::ios::beg);

    std::vector<char> bytes(fileSize);
    stream.read(&bytes[0], fileSize);

    return std::string(&bytes[0], fileSize);
  } else {
    fprintf(stderr, "Can't read from %s\n", file_path.c_str());
    exit(-1);
  }
}

GLuint compileShader(GLenum shader_type, std::string src_file_path) {
  printf("Compiling shader : %s\n", src_file_path.c_str());

  GLuint shader_id = glCreateShader(shader_type);
  std::string vertexShaderCode = loadFileContent(src_file_path);
  char const* srcPtr = vertexShaderCode.c_str();
  GLint len = vertexShaderCode.size();
  glShaderSource(shader_id, 1, &srcPtr, &len);
  glCompileShader(shader_id);

  checkNoGlError();

  GLint status = GL_FALSE;

  int infoLogLength;

  // check shader
  glGetShaderiv(shader_id, GL_COMPILE_STATUS, &status);

  glGetShaderiv(shader_id, GL_INFO_LOG_LENGTH, &infoLogLength);
  if (infoLogLength > 1) {
    std::vector<char> errorMessage(infoLogLength);
    glGetShaderInfoLog(shader_id, infoLogLength, NULL, &errorMessage[0]);
    fprintf(stderr, "%s\n", &errorMessage[0]);
  }

  if (status != GL_TRUE) {
    fprintf(stderr, "ERROR compiling %s\n", src_file_path.c_str());
    fprintf(stderr, "%s\n", srcPtr);
    exit(-1);
  }
  checkNoGlError();

  return shader_id;
}

GLuint loadShaders(std::string vertex_file_path,
                   std::string fragment_file_path) {
  // Compile Vertex Shader
  GLuint vertex_shader = compileShader(GL_VERTEX_SHADER, vertex_file_path);
  GLuint fragment_shader =
      compileShader(GL_FRAGMENT_SHADER, fragment_file_path);

  fprintf(stdout, "Linking program\n");
  GLuint programID = glCreateProgram();
  glAttachShader(programID, vertex_shader);
  glAttachShader(programID, fragment_shader);
  glLinkProgram(programID);
  checkNoGlError();

  // Check the program
  GLint status = GL_FALSE;
  int infoLogLength;

  glGetProgramiv(programID, GL_LINK_STATUS, &status);
  glGetProgramiv(programID, GL_INFO_LOG_LENGTH, &infoLogLength);
  std::vector<char> programErrorMessage(max(infoLogLength, int(1)));
  glGetProgramInfoLog(programID, infoLogLength, NULL, &programErrorMessage[0]);
  fprintf(stderr, "%s\n", &programErrorMessage[0]);

  if (status != GL_TRUE) {
    fprintf(stderr, "ERROR linking program\n");
    exit(-1);
  }

  glDeleteShader(vertex_shader);
  glDeleteShader(fragment_shader);

  checkNoGlError();

  return programID;
}
