subroutine ajchca(para, cham, lpara, lcham, nbent,&
                  maxent, surch)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
    include 'asterfort/assert.h'
    integer :: maxent, nbent, indice, i
    character(len=*) :: para, cham, lpara(maxent), lcham(maxent), surch
!
!     CETTE ROUTINE PERMET D'AJOUTER UN COUPLE (PARAMETRE,NOM_CHAMP)
!     A UNE LISTE LPAIN/LCHIN (OU LPAOUT/LCHOUT)
!
! ----------------------------------------------------------------------
!     IN  : PARA    : NOM DE PARAMETRE CORRESPONDANT A LPARA
!     IN  : CHAM    : NOM DE CHAMP     CORRESPONDANT A LCHAM
!     I/O : LPARA  : TABLEAU DES PARAMETRES
!     I/O : LCHAM  : TABLEAU DES NOMS DES CHAMPS
!     I/O : NBENT  : NOMBRE D'ENTREES
!     IN  : MAXENT : NOMBRE D'ENTREES MAXIMUM
!     IN  : SURCH :  'O' OU 'N'
!
!     SURCH SERT A DETERMINER CE QUE L'ON FAIT SI LE PARAMETRE PARA
!     APPARTIENT DEJA A LPARA.
!
!     ANCIEN       AJOUT    SURCH      NOUVEAU
!     ' '          CH2      O/N        CH2
!     CH1          ' '      O/N        CH1
!     CH1          CH1      O/N        CH1
!     CH1          CH2      N          CH1
!     CH1          CH2      O          CH2
! ----------------------------------------------------------------------
!
!
!     1. RECHERCHE SI LE PARAMETRE EXISTE DEJA
    indice=0
    do 10 i = 1, nbent
        if (lpara(i) .eq. para) then
            indice=i
            goto 20
        endif
10  end do
20  continue
!
!
!     2. IL S'AGIT D'UN NOUVEAU PARAMETRE ON L'AJOUTE :
!     -------------------------------------------------
    if (indice .eq. 0) then
        call assert(nbent.lt.maxent)
        nbent=nbent+1
        lpara(nbent)=para
        lcham(nbent)=cham
        goto 9999
    endif
!
!
!     3. LE NOM DU PARAMETRE EST DEJA DANS LPARA :
!     -------------------------------------------------
!     ANCIEN       AJOUT    SURCH      NOUVEAU
!     ' '          CH2      O/N        CH2
!     CH1          ' '      O/N        CH1
!     CH1          CH1      O/N        CH1
!     CH1          CH2      N          CH1
!     CH1          CH2      O          CH2
!
!     3.1 L'ANCIEN CHAMP ETAIT "BLANC" : ON STOCKE LE NOUVEAU:
    if (lcham(indice) .eq. ' ') then
        lcham(indice)=cham
        goto 9999
    endif
!
!
!     3.2 LE NOUVEAU CHAMP EST "BLANC" : ON NE LE STOCKE PAS
    if (cham .eq. ' ') then
        goto 9999
    endif
!
!
!     3.3 LE NOUVEAU NOM EST LE MEME QUE L'ANCIEN :
    if (lcham(indice) .eq. cham) then
        goto 9999
    endif
!
!
!     3.4 L'ANCIEN NOM DU CHAMP N'ETAIT PAS "BLANC"
!         LE NOUVEAU NOM DU CHAMP NON PLUS ET IL EST DIFFERENT :
    if (surch .eq. 'O') then
        lcham(indice)=cham
    else if (surch.eq.'N') then
    else
        call assert(.false.)
    endif
!
!
9999  continue
end subroutine
