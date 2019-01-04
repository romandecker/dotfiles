module.exports = {
  prompt: ({ inquirer, args }) => {
    if (!args.projectName) {
      return inquirer.prompt({
        type: 'input',
        name: 'projectName',
        message: 'Project name:'
      });
    }
  }
};
