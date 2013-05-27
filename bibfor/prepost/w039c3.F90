subroutine w039c3(carele, modele, ifi, form, titre)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/carelo.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/imprsd.h'
    include 'asterfort/irceme.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    integer :: ifi
    character(len=8) :: carele, modele
    character(len=80) :: titre
    character(len=*) :: form
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!     BUT:
!       IMPRIMER LES REPERES LOCAUX DES ELEMENTS
! ----------------------------------------------------------------------
!     IN MODELE  : MODELE
!     IN CARELE  : CARA_ELEM
! ----------------------------------------------------------------------
!     VARIABLES LOCALES
!
    integer :: iret
    character(len=1) :: nomcmp(3)
    character(len=8) :: typech, sdcarm
    character(len=19) :: chrel1, chrel2, chrel3
    character(len=64) :: nommed
    character(len=80) :: titrz
    logical :: l3d
    data  nomcmp / 'X' , 'Y' , 'Z' /
! ----------------------------------------------------------------------
    call jemarq()
!
    chrel1 = carele//'.REPLO_1'
    chrel2 = carele//'.REPLO_2'
    chrel3 = carele//'.REPLO_3'
!
    call carelo(modele, carele, 'V', chrel1, chrel2,&
                chrel3)
!
    call jeexin(chrel3//'.CELD', iret)
!     IL N'Y A QUE DEUX VECTEURS DANS LE CAS 2D
    if (iret .ne. 0) then
        l3d = .true.
    else
        l3d = .false.
    endif
!     -- IMPRESSION DES CHAMPS DE VECTEURS :
!     -----------------------
!
    if (form .eq. 'MED') then
!     -------------------------
        nommed=chrel1
        sdcarm=' '
        typech='ELEM'
!
        call irceme(ifi, nommed, chrel1, typech, modele,&
                    0, nomcmp, ' ', ' ', 0,&
                    0.d0, 0, 0, 0, sdcarm,&
                    iret)
        call assert(iret.eq.0)
!
        nommed=chrel2
        call irceme(ifi, nommed, chrel2, typech, modele,&
                    0, nomcmp, ' ', ' ', 0,&
                    0.d0, 0, 0, 0, sdcarm,&
                    iret)
        call assert(iret.eq.0)
!
        nommed=chrel3
!
        if (l3d) then
            call irceme(ifi, nommed, chrel3, typech, modele,&
                        0, nomcmp, ' ', ' ', 0,&
                        0.d0, 0, 0, 0, sdcarm,&
                        iret)
            call assert(iret.eq.0)
        endif
!
    else if (form.eq.'RESULTAT') then
!     ---------------------------
        titrz ='1er '//titre
        call imprsd('CHAMP', chrel1, ifi, titrz)
        titrz ='2eme '//titre
        call imprsd('CHAMP', chrel2, ifi, titrz)
        if (l3d) then
            titrz ='3eme '//titre
            call imprsd('CHAMP', chrel3, ifi, titrz)
        endif
    else
        call assert(.false.)
    endif
!
    call detrsd('CHAM_ELEM', chrel1)
    call detrsd('CHAM_ELEM', chrel2)
    if (l3d) call detrsd('CHAM_ELEM', chrel3)
!
    call jedema()
end subroutine
