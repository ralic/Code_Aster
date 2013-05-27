        interface
          subroutine lrmpga(nrofic,ligrel,nochmd,nbma,pgmail,pgmmil,&
     &ntypel,npgmax,indpg,numpt,numord,option,param)
            integer :: npgmax
            integer :: ntypel
            integer :: nbma
            integer :: nrofic
            character(len=19) :: ligrel
            character(*) :: nochmd
            integer :: pgmail(nbma)
            integer :: pgmmil(nbma)
            integer :: indpg(ntypel,npgmax)
            integer :: numpt
            integer :: numord
            character(len=24) :: option
            character(len=8) :: param
          end subroutine lrmpga
        end interface
