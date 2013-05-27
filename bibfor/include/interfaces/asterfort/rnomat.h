        interface
          subroutine rnomat(icesd,icesl,icesv,imap,nomcri,adrma,jtypma&
     &,k,optio,vala,valb,coefpa,nommat)
            integer :: icesd
            integer :: icesl
            integer :: icesv
            integer :: imap
            character(len=16) :: nomcri
            integer :: adrma
            integer :: jtypma
            integer :: k
            character(len=10) :: optio
            real(kind=8) :: vala
            real(kind=8) :: valb
            real(kind=8) :: coefpa
            character(len=8) :: nommat
          end subroutine rnomat
        end interface
