subroutine cutabl(indic, nbliac, ajliai, spliai, resocu,&
                  typope, posit, liaiso)
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
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: indic
    integer :: nbliac
    integer :: ajliai
    integer :: spliai
    integer :: posit
    integer :: liaiso
    character(len=1) :: typope
    character(len=24) :: resocu
!
! ----------------------------------------------------------------------
! ROUTINE APPELEE PAR : ALGOCU
! ----------------------------------------------------------------------
!
! MISE A JOUR DES VECTEURS DE LIAISONS
! LE NOMBRE DE LIAISONS EST MIS A JOUR DANS LA ROUTINE
!
! OUT INDIC  :+1 ON A RAJOUTE UNE LIAISON
!             -1 ON A ENLEVE UNE LIAISON
! I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON CORRECTE DU CALCUL
!              DE LA MATRICE DE CONTACT ACM1AT
! I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
! IN  RESOCU : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  TYPOPE : TYPE D'OPERATION DANS LE VECTEUR DES LIAISONS
!                'A' : AJOUTER UNE LIAISON
!                'S' : SUPPRIMER UNE LIAISON
! IN  POSIT  : POSITION POUR AJOUTER UNE LIAISON DANS LE
!              VECTEUR DES LIAISONS ACTIVES
! IN  LIAISO : INDICE DE LA LIAISON A AJOUTER OU SUPPRIMER
!
!
!
!
!
    integer :: ii
    character(len=1) :: typeaj, typesp
    character(len=19) :: liac
    integer :: jliac
!
! ======================================================================
!
    call jemarq()
!
    liac = resocu(1:14)//'.LIAC'
    call jeveuo(liac, 'E', jliac)
!
! --- INITIALISATION DES VARIABLES TYPE DE CONTACT
!
    typeaj = 'A'
    typesp = 'S'
!
    if (typope .eq. typeaj) then
!
! --- ON AJOUTE UNE LIAISON
!
        indic = 1
        zi(jliac-1+posit) = liaiso
        nbliac = nbliac + 1
    else if (typope.eq.typesp) then
!
! --- ON SUPPRIME UNE LIAISON
!
        indic = -1
        do 30 ii = posit, nbliac - 1
            zi(jliac-1+ii) = zi(jliac-1+ii+1)
30      continue
        nbliac = nbliac - 1
        ajliai = ajliai - 1
    endif
!
! --- MISE A JOUR DE L'INDICATEUR POUR LA FACTORISATION DE LA MATRICE
! --- DE CONTACT
!
    spliai = min(spliai,posit-1)
    spliai = min(spliai,nbliac)
    if (ajliai .lt. 0) then
        ajliai = 0
    endif
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
