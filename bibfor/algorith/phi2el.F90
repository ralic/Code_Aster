subroutine phi2el(modele, carele, mate, accel, phibar,&
                  instap, ve)
    implicit none
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'jeveux.h'
    include 'asterfort/calcul.h'
    include 'asterfort/codent.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mecact.h'
    include 'asterfort/megeom.h'
    include 'asterfort/memare.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: modele, carele, mate, accel, phibar, ve
    real(kind=8) :: instap
! ---------------------------------------------------------------------
!     CALCUL DES VECTEURS ELEMENTAIRES DES FLUX FLUIDES
!
! IN  MODELE  : NOM DU MODELE
! IN  CARELE  : CARACTERISTIQUES DES POUTRES ET COQUES
! IN  MATE    : MATERIAU
! IN  ACCEL  : CHAM_NO DE OU DE DEPL
! IN  INSTAP  : INSTANT DU CALCUL
! VAR VE  : VECTEUR ELEMENTAIRE DE CHARGEMENT THERMIQUE
!
!               & & V E C T ? ? .
!               1 2 3 4 5 6 7 8 9
!
!               POSITION 7-8  : NUMERO DE LA CHARGE
!
!
!
    character(len=8) :: lpain(5), lpaout(1), k8bid, kbid
    character(len=16) :: option
    character(len=19) :: vecel
    character(len=24) :: chgeom, chtime
    character(len=24) :: ligrmo, lchin(5), lchout(1), phib24, ve2
    integer :: ibid, iret
    logical :: prem
    complex(kind=8) :: cbid
!
!-----------------------------------------------------------------------
    integer :: jlve, nbchte
!-----------------------------------------------------------------------
    call jemarq()
!
    vecel=ve
    ve2 = vecel//'.RELR'
    call jeexin(ve2, iret)
!
    if (iret .eq. 0) then
        prem = .true.
        call memare('V', vecel, modele(1:8), mate, carele,&
                    'CHAR_THER')
        call wkvect(ve2, 'V V K24', 1, jlve)
        if (accel(9:14) .eq. '.BIDON') then
            call jeecra(ve2, 'LONUTI', 0, k8bid)
            goto 10
        endif
    else
        prem = .false.
        call jelira(ve2, 'LONUTI', nbchte, k8bid)
        if (nbchte .eq. 0) then
            goto 10
        endif
        call jeveuo(ve2, 'E', jlve)
    endif
!
    ligrmo = modele(1:8)//'.MODELE'
!
    call megeom(modele(1:8), chgeom)
!
    phib24 = phibar
!
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    chtime = '&&VECHME.CH_INST_R'
    call mecact('V', chtime, 'MODELE', ligrmo, 'INST_R  ',&
                1, 'INST   ', ibid, instap, cbid,&
                kbid)
    call mecact('V', '&PHI2M.VEC', 'MODELE', ligrmo, 'TEMP_R  ',&
                1, 'TEMP   ', ibid, 0.d0, cbid,&
                kbid)
    lpain(2) = 'PTEMPSR'
    lchin(2) = chtime
    lpain(3) = 'PACCELR'
    lchin(3) = accel
    lpain(4) = 'PMATERC'
    lchin(4) = mate
    lpaout(1) = 'PVECTTR'
    option = 'CHAR_THER_PHID_R'
    lpain(5) = 'PTEMPER'
    lchin(5) = phib24
!
!
    if (prem) then
!
! ----- CREATION DU VECT_ELEM
!
        lchout(1) = vecel(1:8)//'.VE'
        call codent(1, 'D0', lchout(1) (7:8))
        call calcul('S', option, ligrmo, 5, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
        zk24(jlve) = lchout(1)
        call jeecra(ve2, 'LONUTI', 1, k8bid)
    else
!
! ----- LE VECT_ELEM EXISTE DEJA
!
        lchout(1) = zk24(jlve)
        call calcul('S', option, ligrmo, 5, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
    endif
10  continue
!
! FIN ---------------------------------------------------------------
    call jedema()
end subroutine
