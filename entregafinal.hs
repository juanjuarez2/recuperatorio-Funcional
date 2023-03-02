data Usuario = Usuario{
    nombre :: Nombre,
    seguidos ::[Nombre]
}
type Nombre = String

marsupial = Usuario "@marsupialRengo" ["@don_churrasco", "@titoOk"]
churrasco = Usuario "@don_churrasco" []
tito = Usuario "@titoOk" ["@lapipi", "@marsupialRengo"]
pipi = Usuario "@lapipi" ["@titoOk"]

data Mensaje = Mensaje {
    usuario :: Nombre,
    texto :: Texto,
    favs :: Cantidad 
    }
    type Texto = String
    type Cantidad = Int
    
    unMensaje = Mensaje "@titoOk" "Las personas que viajan en tren se quejan de llenos" 100
    otroMensaje = Mensaje "@lapipi" "En mi mundo todos son un pony" 0
    otroMensajeMas = Mensaje "@lapipi" "y comen arcoiris y su popó son mariposas" 0
    elUltimoMensaje = Mensaje "@titoOk" "No hay problema en cometer errores, el secreto es no pasarlos a producción" 3

valoracion :: Mensaje -> Int
valoracion mensaje = (favs mensaje) -length (texto mensaje)+length (usuario mensaje)



elPrimeroSigueAlSegundo :: Usuario -> Usuario -> Bool
elPrimeroSigueAlSegundo usuario otroUsuario = elem otroUsuario (seguidos usuario)

seguidores :: Usuario -> [Usuario] ->[Usuario]
seguidores usuario usuarios = filter (seguidos usuarios) (nombre usuario) 
-- la idea es filtrar la lista de los seguidos de los usuarios, y devuelva los que siguen al usuario principal y eso serian sus seguidores

favearUnMensaje :: Mensaje -> Mensaje
favearUnMensaje mensaje = mensaje{favs = favs mensaje +1}

editar :: Mensaje -> String ->Mensaje
editar mensaje textoNuevo = mensaje{texto = textoNuevo:texto mensaje}

repipear ::Mensaje -> Mensaje
repipear mensaje  = borrarFavoritos(mensaje) editar(mensaje ":")

borrarFavoritos :: Mensaje -> Mensaje
borrarFavoritos mensaje = mensaje{favoritos = 0}


data Acciones = unaAccion{
    realizar :: Mensaje -> Mensaje 
}

puntoCinco :: Mensaje -> Acciones -> Acciones -> Mensaje
puntoCinco mensaje accion1 accion2 = 
 | valoracion (realizar (accion1)) > valoracion(realizar (accion2)) = realizar accion1 mensaje 
 | otherwise = realizar accion2 mensaje









