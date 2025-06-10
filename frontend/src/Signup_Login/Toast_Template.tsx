
const ToastTemplate: React.FC<{e_mail: string}> = ({e_mail}) => {
    return(
        <div className='w-80'>
            <p>You will recieve an email at <b>{e_mail}</b> shortly 🙌🏿</p>
        </div>
    )
}

export default ToastTemplate
