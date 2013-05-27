        interface
          subroutine lrmmfa(fid,nomamd,nbnoeu,nbmail,grpnoe,gpptnn,&
     &grpmai,gpptnm,nbgrno,nbgrma,typgeo,nomtyp,nmatyp,prefix,infmed,&
     &vecgrm,nbcgrm)
            integer :: fid
            character(*) :: nomamd
            integer :: nbnoeu
            integer :: nbmail
            character(len=24) :: grpnoe
            character(len=24) :: gpptnn
            character(len=24) :: grpmai
            character(len=24) :: gpptnm
            integer :: nbgrno
            integer :: nbgrma
            integer :: typgeo(69)
            character(len=8) :: nomtyp(69)
            integer :: nmatyp(69)
            character(len=6) :: prefix
            integer :: infmed
            character(len=24) :: vecgrm
            integer :: nbcgrm
          end subroutine lrmmfa
        end interface
