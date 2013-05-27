        interface
          subroutine cglect(resu,modele,ndim,option,cas,typfis,nomfis,&
     &fonoeu,chfond,basfon,taillr,conf,lnoff,liss,ndeg)
            character(len=8) :: resu
            character(len=8) :: modele
            integer :: ndim
            character(len=16) :: option
            character(len=16) :: cas
            character(len=8) :: typfis
            character(len=8) :: nomfis
            character(len=24) :: fonoeu
            character(len=24) :: chfond
            character(len=24) :: basfon
            character(len=24) :: taillr
            character(len=8) :: conf
            integer :: lnoff
            character(len=24) :: liss
            integer :: ndeg
          end subroutine cglect
        end interface
