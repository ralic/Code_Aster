        interface
          subroutine caeihm(nomte,axi,perman,mecani,press1,press2,&
     &tempe,dimdef,dimcon,ndim,nno1,nno2,npi,npg,dimuel,iw,ivf1,idf1,&
     &ivf2,idf2,jgano1,iu,ip,ipf,iq,modint)
            character(len=16) :: nomte
            logical :: axi
            logical :: perman
            integer :: mecani(8)
            integer :: press1(9)
            integer :: press2(9)
            integer :: tempe(5)
            integer :: dimdef
            integer :: dimcon
            integer :: ndim
            integer :: nno1
            integer :: nno2
            integer :: npi
            integer :: npg
            integer :: dimuel
            integer :: iw
            integer :: ivf1
            integer :: idf1
            integer :: ivf2
            integer :: idf2
            integer :: jgano1
            integer :: iu(3,18)
            integer :: ip(2,9)
            integer :: ipf(2,2,9)
            integer :: iq(2,2,9)
            character(len=3) :: modint
          end subroutine caeihm
        end interface
