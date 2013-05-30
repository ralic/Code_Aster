subroutine nmext2(noma, champ, nbcmp, nbno, extrch,&
                  extrcp, listno, listcp, chnoeu)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nmexti.h'
    integer :: nbcmp, nbno
    character(len=8) :: noma
    character(len=8) :: extrcp, extrch
    character(len=24) :: listno, listcp
    character(len=19) :: champ, chnoeu
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (EXTRACTION - UTILITAIRE)
!
! EXTRAIRE LES VALEURS - CAS DES CHAMPS AUX NOEUDS
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  CHAMP  : CHAMP OBSERVE
! IN  NBCMP  : NOMBRE DE COMPOSANTES DANS LA SD
! IN  NBNO   : NOMBRE DE NOEUDS DANS LA SD
! IN  EXTRCH : TYPE D'EXTRACTION SUR LE CHAMP
! IN  EXTRCP : TYPE D'EXTRACTION SUR LES COMPOSANTES
! IN  LISTNO : LISTE CONTENANT LES NOEUDS
! IN  LISTCP : LISTE DES COMPOSANTES
! IN  CHNOEU : VECTEUR DE TRAVAIL CHAMPS AUX NOEUDS
!
! ----------------------------------------------------------------------
!
    integer :: nparx
    parameter    (nparx=20)
    real(kind=8) :: valres(nparx)
!
    integer :: jnoeu, jno
    integer :: ino, inor, numnoe
    character(len=8) :: nomnoe
    integer :: ivalcp, nvalcp
    real(kind=8) :: valr, val2r
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES AU CHAMP DE TRAVAIL
!
    call jeveuo(chnoeu, 'E', jnoeu)
    call assert(nbcmp.le.nparx)
!
! --- ACCES LISTE DES NOEUDS
!
    call jeveuo(listno, 'L', jno)
!
! --- BOUCLE SUR LES NOEUDS
!
    do 20 ino = 1, nbno
!
! ----- NOEUD COURANT
!
        numnoe = zi(jno-1+ino)
        call jenuno(jexnum(noma(1:8)//'.NOMNOE', numnoe), nomnoe)
!
! ----- EXTRACTION DES VALEURS AUX NOEUDS
!
        call nmexti(nomnoe, champ, nbcmp, listcp, extrcp,&
                    nvalcp, valres)
!
        if (extrch .eq. 'VALE') then
            inor = ino
        else
            inor = 1
        endif
!
! ----- AFFECTATION DES VALEURS AUX NOEUDS
!
        do 35 ivalcp = 1, nvalcp
            valr = zr(jnoeu+ivalcp-1 +nbcmp*(inor-1))
            val2r = valres(ivalcp)
            if (extrch .eq. 'VALE') then
                zr(jnoeu+ivalcp-1 +nbcmp*(inor-1)) = val2r
            else if (extrch.eq.'MIN') then
                zr(jnoeu+ivalcp-1 +nbcmp*(inor-1)) = min(val2r,valr)
            else if (extrch.eq.'MAX') then
                zr(jnoeu+ivalcp-1 +nbcmp*(inor-1)) = max(val2r,valr)
            else if (extrch.eq.'MAXI_ABS') then
                zr(jnoeu+ivalcp-1 +nbcmp*(inor-1)) = max(abs(val2r), abs(valr))
            else if (extrch.eq.'MINI_ABS') then
                zr(jnoeu+ivalcp-1 +nbcmp*(inor-1)) = min(abs(val2r), abs(valr))
            else if (extrch.eq.'MOY') then
                zr(jnoeu+ivalcp-1 +nbcmp*(inor-1)) = valr+val2r
            else
                call assert(.false.)
            endif
35      continue
20  end do
!
! --- CALCUL DE LA MOYENNE SUR LES NOEUDS
!
    if (extrch .eq. 'MOY') then
        inor = 1
        do 25 ivalcp = 1, nvalcp
            valr = zr(jnoeu+ivalcp-1 +nbcmp*(inor-1))
            if (nbno .ne. 0) then
                zr(jnoeu+ivalcp-1 +nbcmp*(inor-1)) = valr/nbno
            endif
25      continue
    endif
!
    call jedema()
!
end subroutine
