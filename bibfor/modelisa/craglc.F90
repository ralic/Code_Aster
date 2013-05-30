subroutine craglc(long, ligrch)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    include 'jeveux.h'
!
    include 'asterfort/agligr.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedupo.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=19) :: ligrch
! ---------------------------------------------------------------------
!     CREATION OU EXTENSION DU LIGREL DE CHARGE LIGRCH
!     D'UN NOMBRE DE TERMES EGAL A LONG
!
!     LONG DOIT ETRE > 0
!
!----------------------------------------------------------------------
!  LONG          - IN   - I    - : NOMBRE DE GRELS A RAJOUTER A LIGRCH-
!----------------------------------------------------------------------
!  LIGRCH        - IN   - K19  - : NOM DU LIGREL DE CHARGE
!                - JXVAR-      -
!----------------------------------------------------------------------
!
!
! --------- VARIABLES LOCALES ------------------------------------------
    character(len=8) :: mod
    character(len=8) :: k8bid
    character(len=1) :: k1bid
! --------- FIN  DECLARATIONS  VARIABLES LOCALES ----------------------
!
!-----------------------------------------------------------------------
    integer :: ibid, idlgns, idnbno, ier, iret, lonema, long
    integer :: longma, longut, lonlig, lont, nbeldi, nbelma, nbelut
    integer :: nbmata, nbnomx
!-----------------------------------------------------------------------
    call jemarq()
    if (long .le. 0) then
        call u2mess('F', 'MODELISA4_37')
    endif
!
! --- ON CREE LIGREL DE CHARGE S'IL N'EXISTE PAS ---
!
    call jeexin(ligrch//'.LIEL', iret)
    if (iret .eq. 0) then
        call jecrec(ligrch//'.LIEL', 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                    long)
        lonlig = 2*long
        call jeecra(ligrch//'.LIEL', 'LONT', lonlig, ' ')
!
        call jecrec(ligrch//'.NEMA', 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                    long)
        lonema = 4*long
        call jeecra(ligrch//'.NEMA', 'LONT', lonema, ' ')
!
        call wkvect(ligrch//'.LGNS', 'G V I', 2*lonema, idlgns)
!
! --- MODELE ASSOCIE AU LIGREL DE CHARGE ---
!
        call dismoi('F', 'NOM_MODELE', ligrch(1:8), 'CHARGE', ibid,&
                    mod, ier)
!
! --- MAILLAGE ASSOCIE AU MODELE ---
!
        call jedupo(mod//'.MODELE    .LGRF', 'G', ligrch//'.LGRF', .false.)
!
        call wkvect(ligrch//'.NBNO', 'G V I', 1, idnbno)
        zi(idnbno) = 0
    endif
!
! --- NOMBRE MAX D'ELEMENTS DE LA COLLECTION LIGRCH ---
!
    call jelira(ligrch//'.NEMA', 'NMAXOC', nbelma, k1bid)
!
! --- NOMBRE DE MAILLES TARDIVES DU LIGREL DE CHARGE ---
!
    call dismoi('F', 'NB_MA_SUP', ligrch, 'LIGREL', nbmata,&
                k8bid, ier)
!
! --- NOMBRE D'ELEMENTS DEJA AFFECTES DE LA COLLECTION LIGRCH ---
!
    call jelira(ligrch//'.NEMA', 'NUTIOC', nbelut, k1bid)
!
! --- NOMBRE D'ELEMENTS DISPONIBLES DE LA COLLECTION LIGRCH ---
!
    nbeldi = nbelma - nbelut
!
! --- LONGUEUR TOTALE DE LA COLLECTION LIGRCH ---
!
    call jelira(ligrch//'.NEMA', 'LONT', lont, k1bid)
!
! --- NOUVEAU NOMBRE D'OBJETS DE LA COLLECTION LIGRCH ---
!
    longut = nbelut + long
!
! --- MAJORANT DE LA NOUVELLE LONGUEUR DE LA COLLECTION LIGRCH.NEMA ---
!
    call dismoi('F', 'NB_NO_MAX', '&', 'CATALOGUE', nbnomx,&
                k8bid, ier)
    longma = (nbnomx+1)*longut
!
! --- VERIFICATION DE L'ADEQUATION DE LA TAILLE DU LIGREL DE ---
! --- CHARGE A SON AFFECTATION PAR LES MAILLES TARDIVES DUES ---
! --- AUX RELATIONS LINEAIRES                                ---
!
    if (long .gt. nbeldi .or. longma .gt. lont) then
!
! ---       LA TAILLE DU LIGREL DE CHARGE EST INSUFFISANTE  ---
! ---       ON LA REDIMENSIONNE DE MANIERE ADEQUATE         ---
        call agligr(longut, ligrch)
    endif
!
    call jedema()
end subroutine
