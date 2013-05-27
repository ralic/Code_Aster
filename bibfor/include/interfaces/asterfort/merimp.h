        interface
          subroutine merimp(modele,carele,mate,comref,compor,carcri,&
     &iterat,sddyna,valinc,solalg,caco3d,nbin,lpain,lchin)
            integer :: nbin
            character(len=24) :: modele
            character(len=24) :: carele
            character(*) :: mate
            character(len=24) :: comref
            character(len=24) :: compor
            character(len=24) :: carcri
            integer :: iterat
            character(len=19) :: sddyna
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=24) :: caco3d
            character(len=8) :: lpain(nbin)
            character(len=19) :: lchin(nbin)
          end subroutine merimp
        end interface
