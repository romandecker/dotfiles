module.exports = {
  prompt: async ({ inquirer, args }) => {
    const answers = {};
    if (!args.projectName) {
      Object.assign(
        answers,
        await inquirer.prompt({
          type: 'input',
          name: 'projectName',
          message: 'Project name:'
        })
      );
    }

    // Object.assign(
    //   answers,
    //   await inquirer.prompt({
    //     type: 'checkbox',
    //     name: 'targets',
    //     message: 'Target platform(s):',
    //     choices: ['Node', 'Browser'],
    //     default: ['Node'],
    //     validate: answers => answers.length >= 1 || 'Must select at least one'
    //   })
    // );

    return answers;
  }
};
