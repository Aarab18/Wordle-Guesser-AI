# 🤖 AI Wordle Guesser

A sophisticated, interactive web game where you challenge an intelligent AI to guess your secret 5-letter word. Built with modern web technologies and a focus on a clean, polished user experience.


---

## 📖 Table of Contents

-   [Key Features](#-key-features)
-   [Technologies Used](#-technologies-used)
-   [Local Setup & Installation](#-local-setup--installation)
-   [Deployment to GitHub Pages](#-deployment-to-github-pages)
-   [How to Contribute](#-how-to-contribute)
-   [License](#-license)

---

## ✨ Key Features

-   **Intelligent AI Opponent:** The AI uses a sophisticated algorithm to make the most informative guess possible, narrowing down the word list with each turn.
-   **Selectable Difficulty:** Choose between **Normal** mode for a standard challenge or **Hard** mode, where the AI employs a more strategic guessing algorithm to solve the puzzle faster.
-   **Interactive Feedback System:** A simple and intuitive input system (`G` `Y` `B`) for providing clues to the AI.
-   **Dynamic UI & Animations:** Smooth tile-flipping animations, celebratory confetti on a win, and subtle sound effects create an engaging and polished user experience.
-   **"Teach Me" Feature:** If your word isn't in the AI's dictionary, the game allows you to add it, making the AI smarter for the next game.
-   **Shareable Results:** Easily copy your game's results as a classic emoji grid to share with friends.
-   **Fully Responsive:** A clean and modern interface that works seamlessly on desktops, tablets, and mobile devices.

---

## 🛠️ Technologies Used

-   **Frontend:** HTML5, CSS3, JavaScript (ES6+)
-   **Styling:** [Tailwind CSS](https://tailwindcss.com/)
-   **Audio:** [Tone.js](https://tonejs.github.io/) for web audio effects.
-   **Deployment:** [GitHub Pages](https://pages.github.com/)

---

## 🚀 Local Setup & Installation

To run this project on your local machine, follow these steps:

1.  **Download the project files:**
    Make sure you have `index.html`, `style.css`, and `script.js` in the same folder.

2.  **Open `index.html` in your browser:**
    You can simply double-click the `index.html` file. For the best experience, it's recommended to use a local server. If you have VS Code, the [Live Server](https://marketplace.visualstudio.com/items?itemName=ritwickdey.LiveServer) extension is an excellent choice.

---

## 🌐 Deployment to GitHub Pages

You can easily deploy your own version of this game for free using GitHub Pages.

1.  **Create a new repository** on GitHub (e.g., `Wordle-Guesser`).

2.  **Initialize Git** in your local project folder, add the files, commit them, and push them to your new repository:
    ```bash
    # Make sure you are in your project folder
    git init
    git add .
    git commit -m "Initial commit"
    git branch -M main
    git remote add origin https://github.com/YOUR_USERNAME/YOUR_REPOSITORY_NAME.git
    git push -u origin main
    ```
    *(Remember to replace the URL with your actual repository URL)*

3.  **Enable GitHub Pages:**
    -   In your repository on GitHub, go to **Settings**.
    -   In the left sidebar, click on **Pages**.
    -   Under "Build and deployment", set the **Source** to `Deploy from a branch`.
    -   Under "Branch", select `main` and `/ (root)`, then click **Save**.

4.  Your site will be live in a few minutes at 'https://YOUR_USERNAME.github.io/YOUR_REPOSITORY_NAME.

---

## 🤝 How to Contribute

Contributions are welcome! If you have ideas for new features, improvements, or bug fixes, please feel free to:

1.  **Fork the repository.**
2.  **Create a new branch** (`git checkout -b feature/AmazingFeature`).
3.  **Make your changes** and commit them (`git commit -m 'Add some AmazingFeature'`).
4.  **Push to the branch** (`git push origin feature/AmazingFeature`).
5.  **Open a Pull Request.**

---

## 📄 License

This project is distributed under the MIT License. See the `LICENSE` file for more information.
