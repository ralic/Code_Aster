        interface
          subroutine crgdm(imate,compor,lambda,deuxmu,lamf,deumuf,gt,&
     &gc,gf,seuil,alpha,alfmc,ep,lrgm,ipg,ther,tref,dtmoy,dtgra,tmoym,&
     &tgram,alph)
            integer :: imate
            character(len=16) :: compor
            real(kind=8) :: lambda
            real(kind=8) :: deuxmu
            real(kind=8) :: lamf
            real(kind=8) :: deumuf
            real(kind=8) :: gt
            real(kind=8) :: gc
            real(kind=8) :: gf
            real(kind=8) :: seuil
            real(kind=8) :: alpha
            real(kind=8) :: alfmc
            real(kind=8) :: ep
            logical :: lrgm
            integer :: ipg
            logical :: ther
            real(kind=8) :: tref
            real(kind=8) :: dtmoy
            real(kind=8) :: dtgra
            real(kind=8) :: tmoym
            real(kind=8) :: tgram
            real(kind=8) :: alph
          end subroutine crgdm
        end interface
