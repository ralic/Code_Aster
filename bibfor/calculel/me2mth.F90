subroutine me2mth(modelz, nchar, lchar, matez, caraz,&
                  timez, chtnz, vecelz)
    implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
!
    character(len=*) :: modelz, chtnz, caraz, matez, vecelz, timez
    character(len=*) :: lchar(*)
    character(len=8) :: modele, cara
    character(len=8) :: lcharz
    character(len=19) :: vecel
    character(len=24) :: mate, time
    integer :: nchar
! ......................................................................
!     BUT:
!         CALCUL DE TOUS LES SECONDS MEMBRES ELEMENTAIRES PROVENANT
!         DES CHARGES_THERMIQUES
!
!     ENTREES:
!
!     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
!        MODELZ : NOM DU MODELE
!        NCHAR  : NOMBRE DE CHARGES
!        LCHAR  : LISTE DES CHARGES
!        MATEZ  : CHAM_MATER
!        CARAZ  : CARAC_ELEM
!        TIMEZ  : CHAMPS DE TEMPSR
!        CHTNZ  : CHAM_NO DE TEMPERATURE A L'INSTANT TN
!        VECELZ : NOM DU VEC_ELE (N RESUELEM) PRODUIT
!                 SI VECEL EXISTE DEJA, ON LE DETRUIT.
!
!     SORTIES:
!     SONT TRAITES ACTUELLEMENT LES CHAMPS:
!        LCHAR(ICHA)//'.CHTH.CIMPO     ' : TEMPERATURE IMPOSEE
!        LCHAR(ICHA)//'.CHTH.SOURE     ' : SOURCE REPARTIE
!        LCHAR(ICHA)//'.CHTH.GRAIN     ' : GRADIENT INITIAL
!        LCHAR(ICHA)//'.CHTH.FLURE     ' : FLUX NORMAL REPARTI
!        LCHAR(ICHA)//'.CHTH.FLUR2     ' : FLUX NORMAL REPARTI (VECTEUR)
!        LCHAR(ICHA)//'.CHTH.T_EXT     ' : TEMPERATURE EXTERIEURE
!
! ......................................................................
!
!
!
!
    logical(kind=1) :: lfonc
    character(len=8) :: lpain(5), lpaout(1), k8bid
    character(len=16) :: option
    character(len=24) :: lchin(5), lchout(1), ligrmo, ligrch
    character(len=24) :: chgeom
    integer :: icha, iret, ilires
    character(len=8), pointer :: nomo(:) => null()
!
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    modele = modelz
    mate = matez
    cara = caraz
    vecel = vecelz
    time = timez
    lcharz = lchar(1)
    call megeom(modele, chgeom)
!
    call jeexin(vecel//'.RERR', iret)
    if (iret .gt. 0) then
        call jedetr(vecel//'.RERR')
        call jedetr(vecel//'.RELR')
    endif
    call memare('G', vecel, modele, ' ', cara,&
                'CHAR_THER')
!
    lpaout(1) = 'PVECTTR'
    lchout(1) = vecel(1:8)//'.VE000'
    ilires = 0
!
!     BOUCLE SUR LES CHARGES POUR CALCULER :
!         ( CHAR_THER_TEXT_F , ISO_FACE ) SUR LE MODELE
!         ( CHAR_THER_FLUN_F , ISO_FACE ) SUR LE MODELE
!         ( CHAR_THER_SOUR_F , ISO      ) SUR LE MODELE
!         ( CHAR_THER_SOUR_F , SOURCE_NO) SUR LE LIGREL(CHARGE)
!         ( THER_DDLI_F      , CAL_TI   ) SUR LE LIGREL(CHARGE)
!
!
    if (nchar .ne. 0) then
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PTEMPSR'
        lchin(2) = time
        if (modele .ne. '        ') then
            ligrmo = modele//'.MODELE'
        else
            lcharz = lchar(1)
            call jeveuo(lcharz//'.CHTH      .NOMO', 'L', vk8=nomo)
            ligrmo = nomo(1)//'.MODELE'
        endif
        do icha = 1, nchar
            lcharz = lchar(icha)
            call dismoi('TYPE_CHARGE', lcharz, 'CHARGE', repk=k8bid)
            if (k8bid(5:7) .eq. '_FO') then
                lfonc = .true.
            else
                lfonc = .false.
            endif
!
            ligrch = lcharz//'.CHTH.LIGRE      '
!  =====================================================================
!           --  ( CHAR_THER_TEXT_F , ISO_FACE ) SUR LE MODELE
            call exisd('CHAMP_GD', ligrch(1:13)//'.COEFH', iret)
            if (iret .ne. 0) then
                if (lfonc) then
                    option = 'CHAR_THER_TEXT_F'
                    lpain(3) = 'PT_EXTF'
                    lpain(4) = 'PCOEFHF'
                else
                    option = 'CHAR_THER_TEXT_R'
                    lpain(3) = 'PT_EXTR'
                    lpain(4) = 'PCOEFHR'
                endif
                lchin(3) = ligrch(1:13)//'.T_EXT     '
                lchin(4) = ligrch(1:13)//'.COEFH     '
                lpain(5) = 'PTEMPER'
                lchin(5) = chtnz
                ilires = ilires + 1
                call codent(ilires, 'D0', lchout(1) (12:14))
                call calcul('S', option, ligrmo, 5, lchin,&
                            lpain, 1, lchout, lpaout, 'G',&
                            'OUI')
                call reajre(vecel, lchout(1), 'G')
            endif
!  =====================================================================
!           --  ( CHAR_THER_FLUN_F , ISO_FACE ) SUR LE MODELE
            call exisd('CHAMP_GD', ligrch(1:13)//'.FLURE', iret)
            if (iret .ne. 0) then
                if (lfonc) then
                    option = 'CHAR_THER_FLUN_F'
                    lpain(3) = 'PFLUXNF'
                else
                    option = 'CHAR_THER_FLUN_R'
                    lpain(3) = 'PFLUXNR'
                endif
                lchin(3) = ligrch(1:13)//'.FLURE     '
                ilires = ilires + 1
                call codent(ilires, 'D0', lchout(1) (12:14))
                call calcul('S', option, ligrmo, 3, lchin,&
                            lpain, 1, lchout, lpaout, 'G',&
                            'OUI')
                call reajre(vecel, lchout(1), 'G')
            endif
!  =====================================================================
!           --  ( CHAR_THER_FLUX_  , ISO_FACE ) SUR LE MODELE
            call exisd('CHAMP_GD', ligrch(1:13)//'.FLUR2', iret)
            if (iret .ne. 0) then
                if (lfonc) then
                    option = 'CHAR_THER_FLUX_F'
                    lpain(3) = 'PFLUXVF'
                else
                    option = 'CHAR_THER_FLUX_R'
                    lpain(3) = 'PFLUXVR'
                endif
                lchin(3) = ligrch(1:13)//'.FLUR2     '
                ilires = ilires + 1
                call codent(ilires, 'D0', lchout(1) (12:14))
                call calcul('S', option, ligrmo, 3, lchin,&
                            lpain, 1, lchout, lpaout, 'G',&
                            'OUI')
                call reajre(vecel, lchout(1), 'G')
            endif
!  =====================================================================
!           --   ( CHAR_THER_SOUR_F , ISO    )  SUR LE MODELE
            call exisd('CHAMP_GD', ligrch(1:13)//'.SOURE', iret)
            if (iret .ne. 0) then
                if (lfonc) then
                    option = 'CHAR_THER_SOUR_F'
                    lpain(3) = 'PSOURCF'
                else
                    option = 'CHAR_THER_SOUR_R'
                    lpain(3) = 'PSOURCR'
                endif
                lchin(3) = ligrch(1:13)//'.SOURE     '
                ilires = ilires + 1
                call codent(ilires, 'D0', lchout(1) (12:14))
                call calcul('S', option, ligrmo, 3, lchin,&
                            lpain, 1, lchout, lpaout, 'G',&
                            'OUI')
                call reajre(vecel, lchout(1), 'G')
            endif
!  =====================================================================
!           --   ( CHAR_THER_GRAI_  ,  ISO_VOLU  ) SUR LE   MODELE
            call exisd('CHAMP_GD', ligrch(1:13)//'.GRAIN', iret)
            if (iret .ne. 0) then
                if (lfonc) then
                    option = 'CHAR_THER_GRAI_F'
                    lpain(3) = 'PGRAINF'
                else
                    option = 'CHAR_THER_GRAI_R'
                    lpain(3) = 'PGRAINR'
                endif
                lchin(3) = ligrch(1:13)//'.GRAIN     '
                lpain(4) = 'PMATERC'
                lchin(4) = mate
                ilires = ilires + 1
                call codent(ilires, 'D0', lchout(1) (12:14))
                call calcul('S', option, ligrmo, 4, lchin,&
                            lpain, 1, lchout, lpaout, 'G',&
                            'OUI')
                call reajre(vecel, lchout(1), 'G')
            endif
!  =====================================================================
!           --   ( THER_DDLI_F    , CAL_TI   )  SUR LE LIGREL(CHARGE)
            call exisd('CHAMP_GD', ligrch(1:13)//'.CIMPO', iret)
            if (iret .ne. 0) then
                if (lfonc) then
                    option = 'THER_DDLI_F'
                    lpain(3) = 'PDDLIMF'
                else
                    option = 'THER_DDLI_R'
                    lpain(3) = 'PDDLIMR'
                endif
                lchin(3) = ligrch(1:13)//'.CIMPO     '
                ilires = ilires + 1
                call codent(ilires, 'D0', lchout(1) (12:14))
                call calcul('S', option, ligrch, 3, lchin,&
                            lpain, 1, lchout, lpaout, 'G',&
                            'OUI')
                call reajre(vecel, lchout(1), 'G')
            endif
        end do
    endif
!
    call jedema()
end subroutine
