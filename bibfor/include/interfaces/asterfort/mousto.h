        interface
          subroutine mousto(guidag,dimtub,voltub,tubuse,dimobs,volobs,&
     &obsuse,rcray,rcarte,sect,arete,arete2,ns,obcont,epais,ecray,resu,&
     &denc,perce)
            character(len=8) :: guidag
            integer :: dimtub
            real(kind=8) :: voltub(*)
            real(kind=8) :: tubuse(*)
            integer :: dimobs
            real(kind=8) :: volobs(*)
            real(kind=8) :: obsuse(*)
            real(kind=8) :: rcray
            real(kind=8) :: rcarte
            real(kind=8) :: sect(*)
            real(kind=8) :: arete
            real(kind=8) :: arete2
            integer :: ns
            character(len=8) :: obcont
            real(kind=8) :: epais
            real(kind=8) :: ecray
            character(len=19) :: resu
            real(kind=8) :: denc
            real(kind=8) :: perce
          end subroutine mousto
        end interface
