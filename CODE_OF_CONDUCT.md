# Code of Conduct for Writing New Code

## Purpose

The purpose of this code of conduct is to establish guidelines and expectations for writing new code within our project. These guidelines aim to create a collaborative and inclusive environment for all contributors.

## General Principles

1. **Clarity and Readability:**
   - Write code that is clear, concise, and easy to understand.
   - Use meaningful variable and function names.
   - Comment code when necessary to explain complex logic or decision points.

2. **Consistency:**
   - Follow established coding styles and conventions in the project.
   - Maintain a consistent coding style throughout the codebase.

3. **Modularity and Reusability:**
   - Break down code into modular components.
   - Encapsulate functionality in functions or classes for reusability.

4. **Testing:**
   - Write unit tests for new code to ensure its correctness in [`tests/`](/tests/) folder.
   - Test your code in different scenarios to cover edge cases.
  
To maintain the code over the time, please follow the instructions below in order to remain comprehensive, and keep good practices in mind :

**``Functionalize``** : **1 function = 1 action or goal**. Otherwhise, it is so difficult to understand the aim of what you are doing. No more scripts with more than 1000 lines.

**``Names``** : **names has to be short but self-sufficient and comprehensive**. Keep the same case style for the whole code.

**``Parameters``** : **no hardcode**, everything has to be parametered and always think about the scalability of your code. It makes your code robust.

**`Language`** : **Use english** and keep the same language during the code

**`Doctring`** : Please **make, create or update the docstring**. It is compulsory to inform any developers, user of the package.