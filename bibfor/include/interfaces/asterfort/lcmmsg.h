        interface
          subroutine lcmmsg(nomfam,nbsys,nusys,pgl2,mus,ng,mg,ir,q)
            character(len=16) :: nomfam
            integer :: nbsys
            integer :: nusys
            real(kind=8) :: pgl2(3,3)
            real(kind=8) :: mus(6)
            real(kind=8) :: ng(3)
            real(kind=8) :: mg(3)
            integer :: ir
            real(kind=8) :: q(3,3)
          end subroutine lcmmsg
        end interface
