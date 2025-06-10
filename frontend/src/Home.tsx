import styles from './css/Home.module.css'
import { Link } from 'react-router-dom';


const Home = () => {
    return (
        <div className={styles.home}>
            <main className={`${styles.mainWindow}`}>
                <section className={`${styles.welcomeTextAndSubtitle} ${styles.roboto_thin_italic}`}>
                    <WelcomeText />
                    <EditorButtons />
                </section>
                <section className={`${styles.right}`}>
                    <h1>😆😆😆😆😆😆😆</h1>
                </section>
            </main>
        </div>
    )
}


function WelcomeText() {
    return (
        <>
            <div className={styles.welcometext}>
                <p>Real Time</p>
                <p>Collabarative Editor</p>
            </div>
            <br></br>
            <p className={styles.fira_light_italic}> Written in Haskell and React</p>
        </>
    )
}

function EditorButtons() {
    return (
        <div className={styles.editorbuttons}>
            <Link to='/editor'>Try Basic Editor</Link>
            <Link to='/collaborate'>Collaborate</Link>
        </div>
    )
}

export default Home;
