subroutine verif_affe(modele,sd)
    implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterc/indik8.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/assert.h"
#include "asterfort/verif_affe_carte.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
!
    character(len=*), intent(in) :: modele
    character(len=*), intent(in), optional :: sd
!
!-----------------------------------------------------------------------
!   But :
!     Emettre des alarmes concernant les affectations douteuses dans les
!     differentes cartes des SD charge et cara_elem
!
!   Entrees:
!     modele     :  sd_modele
!     sd         :  sd_charge ou sd_cara_elem
!
!-----------------------------------------------------------------------
    character(len=8) :: sd_, modele_
    character(len=19) :: carte, ligrmo
    character(len=24) :: typres
    character(len=80) :: comment
    integer :: n1,k,iret
    character(len=5)  :: l_cart_char_meca(26), l_cart_char_ther(10)
    character(len=8)  :: l_cart_cara_elem(14)
    character(len=80) :: l_comm_char_meca(26), l_comm_char_ther(10), l_comm_cara_elem(14)

!-----------------------------------------------------------------------
!
    call jemarq()

!   -- cartes des sd_char_meca :
!   -----------------------------
    l_cart_char_meca = (/ &
    'EFOND' , 'EPSIN' , 'F1D1D' , 'F1D2D' , 'F1D3D' , 'F2D2D' , 'F2D3D' , 'F3D3D' , 'FCO2D' , &
    'FCO3D' , 'FELEC' , 'FL101' , 'FL102' , 'FLUX ' , 'FORNO' , 'IMPE ' , 'ONDE ' , 'ONDPL' , &
    'ONDPR' , 'PESAN' , 'PREFF' , 'PRESS' , 'ROTAT' , 'SIGIN' , 'SIINT' , 'VNOR ' /)

    l_comm_char_meca = ' '
    l_comm_char_meca(22) = 'Chargement provenant du mot cle PRES_REP'

!   -- cartes des sd_char_ther :
!   -----------------------------
    l_cart_char_ther = (/ &
    'SOURE', 'COEFH', 'FLUNL', 'SOUNL', 'FLUR2', 'FLURE', 'GRAIN', 'HECHP', 'RAYO ', 'T_EXT' /)

    l_comm_char_ther = ' '
    l_comm_char_ther(1) = 'Chargement provenant du mot cle SOURCE'

!   -- cartes des sd_cara_elem :
!   -----------------------------
    l_cart_cara_elem = (/ &
    'CARGENBA', 'CARMASSI', 'CARCABLE', 'CARCOQUE', 'CARDISCK', 'CARARCPO', 'CARGENPO', &
    'CARDISCM', 'CARORIEN', 'CARDISCA', 'CVENTCXF', 'CARPOUFL', 'CARGEOPO', 'CARDINFO' /)

    l_comm_cara_elem = ' '
    l_comm_cara_elem(1)  = 'Caracteristiques provenant du mot cle BARRE'
    l_comm_cara_elem(2)  = 'Caracteristiques provenant du mot cle MASSIF'
    l_comm_cara_elem(3)  = 'Caracteristiques provenant du mot cle CABLE'
    l_comm_cara_elem(4)  = 'Caracteristiques provenant du mot cle COQUE'
    l_comm_cara_elem(5)  = 'Caracteristiques provenant du mot cle DISCRET de type raideur'
    l_comm_cara_elem(6)  = 'Caracteristiques provenant des POUTRES courbes'
    l_comm_cara_elem(7)  = 'Caracteristiques provenant du mot cle POUTRE'
    l_comm_cara_elem(8)  = 'Caracteristiques provenant du mot cle DISCRET de type masse'
    l_comm_cara_elem(9)  = 'Caracteristiques provenant du mot cle ORIENTATION'
    l_comm_cara_elem(10) = 'Caracteristiques provenant du mot cle DISCRET d''amortissement'
    l_comm_cara_elem(11) = 'Caracteristiques provenant du mot cle FCX'
    l_comm_cara_elem(12) = 'Caracteristiques provenant du mot cle POUTRE_FLUI'
    l_comm_cara_elem(13) = 'Caracteristiques provenant du mot cle POUTRE (geometrie)'
    l_comm_cara_elem(14) = 'Caracteristiques provenant du mot cle DISCRET (information)'

    modele_=modele
    ligrmo=modele_//'.MODELE'

!   -- boucle sur les cartes de la SD :
!   ---------------------------------------
    if (present(sd)) then
        sd_=sd
        call gettco(sd_, typres)

        if (typres.eq.'CHAR_MECA') then
            n1=size(l_cart_char_meca)
            do k=1,n1
                carte=sd_//'.CHME.'//l_cart_char_meca(k)
                comment=l_comm_char_meca(k)
                call exisd('CARTE', carte, iret)
                if (iret .gt. 0)  call verif_affe_carte(ligrmo,carte,comment)
            enddo

        elseif (typres.eq.'CHAR_THER') then
            n1=size(l_cart_char_ther)
            do k=1,n1
                carte=sd_//'.CHTH.'//l_cart_char_ther(k)
                comment=l_comm_char_ther(k)
                call exisd('CARTE', carte, iret)
                if (iret .gt. 0)  call verif_affe_carte(ligrmo,carte,comment)
            enddo

        elseif (typres.eq.'CARA_ELEM') then
            n1=size(l_cart_cara_elem)
            do k=1,n1
                carte=sd_//'.'//l_cart_cara_elem(k)
                comment=l_comm_cara_elem(k)
                call exisd('CARTE', carte, iret)
                if (iret .gt. 0)  call verif_affe_carte(ligrmo,carte,comment)
            enddo

        else
            write(6,*) 'AJACOT A faire ... typres=',sd,typres
            ASSERT(.false.)
        endif

    endif

    call jedema()
end subroutine
