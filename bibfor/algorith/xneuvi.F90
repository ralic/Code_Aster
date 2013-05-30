subroutine xneuvi(narz, nar, nbno, tabdir, scorno,&
                  noeud, nliseq)
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    integer :: narz, nar, nbno
    integer :: tabdir(narz, 2), scorno(2*narz), noeud(2*narz)
    character(len=19) :: nliseq
! TOLE CRS_1404
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
!
! ROUTINE XFEM
!
! SERT A CREER POUR UNE FISSURE DONNEE, LES GROUPES D'ARETES VITALES
!      CONNECTEES A UN NOEUD
!
! FISS.CONNECTANT :
!      CHAQUE NOEUD RELIÉ A PLUS D'UNE ARETE VITALE (SCORE(NOEUD) > 1)
!      EST STOQUÉ ICI ET DEFINI UN GROUPE D'ARETES
! FISS.CONNECTES :
!      POUR CHAQUE NOEUD CONNECTANT, ON STOQUE ICI LES NOEUDS RELIÉS
!      QUI FORMENT LES ARETES VITALE
!
! TRAVAIL EFFECTUE EN COLLABORATION AVEC L'I.F.P.
!
! ----------------------------------------------------------------------
!
! IN  NARZ   : NOMBRE D'ARETES COUPEES
! IN  NAR    : NBRE D'ARETES COUPEES NON HYPERSTATIQUES (NH DANS XRELL2)
! IN  NBNO   : NOMBRE DE NOEUDS APARTENANTS AU MOINS A UNE ARETE COUPEE
! IN  TABDIR : TABLEAU DES NOEUDS DES ARETES NH
! IN  SCORNO : REDONE LE SCORE DES NOEUDS DES ARETES
! IN  NOEUD  : REDONE LE NUMERO GLOBAL DES NOEUDS DES ARETES
! IN  NLISEQ : NOM DE LA LISTE DE RELATION D'EGALITE (SERT A
!              RECUPERER LE NOM DE LA FISSURE)
!
!
!
!
    integer :: nnovit, nncone, novit(nbno)
    integer :: i, k, ia
    integer :: jcntan, jcntes
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nnovit = 0
    nncone = 0
!
! --- ON COMPTE ET ON SELECTIONNE LES NOEUDS CONECTANTS
    do 10 i = 1, nbno
        if (scorno(i) .gt. 1) then
            nnovit = nnovit + 1
            novit(nnovit) = i
        endif
10  end do
!
! --- ON CONSTRUIT LE VECTEUR DES NOEUDS CONNECTANTS
    if (nnovit .ne. 0) then
        call wkvect(nliseq(1:8)//'.CONNECTANT', 'G V I', 3*nnovit, jcntan)
        do 20 i = 1, nnovit
! --- POUR CHAQUE NOEUD VITAL, ON STOQUE SON NUMERO, LE NOMBRE DE NOEUDS
! --- QU'IL CONNECTE ET LEPOINTEUR SUR LES NOEUDS QU'IL CONECTE
            zi(jcntan-1+3*(i-1)+1) = noeud(novit(i))
            zi(jcntan-1+3*(i-1)+2) = scorno(novit(i))
            zi(jcntan-1+3*(i-1)+3) = nncone
            nncone = nncone + scorno(novit(i))
20      continue
! --- ON CONSTRUIT LE VECTEUR DES NOEUDS CONNECTÉS
        call wkvect(nliseq(1:8)//'.CONNECTES ', 'G V I', nncone, jcntes)
        k=0
        do 30 i = 1, nnovit
! --- POUR CHAQUE NOEUD VITAL, ON STOQUE SES NOEUDS CONECTÉS
            do 40 ia = 1, nar
                if (tabdir(ia,1) .eq. novit(i)) then
                    k = k+1
                    zi(jcntes-1+k) = noeud(tabdir(ia,2))
                else if (tabdir(ia,2).eq.novit(i)) then
                    k = k+1
                    zi(jcntes-1+k) = noeud(tabdir(ia,1))
                endif
40          continue
30      continue
    endif
!
    call jedema()
!
end subroutine
