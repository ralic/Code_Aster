subroutine codlet(entier, cadre, chaine)
    implicit none
    integer :: entier
    character(len=*) :: cadre, chaine
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!   ------------------------------------------------------------------
!   CODAGE D'UN ENTIER EN BASE 36 DANS UNE CHAINE DE CARACTERE
!   ------------------------------------------------------------------
! IN  ENTIER : IS    : ENTIER A CONVERTIR EN CHAINE
! IN  CADRE  : CH(*) : TYPE DE CADRAGE
!          D     CADRAGE A DROITE
!          D0    CADRAGE A DROITE ET ON COMPLETE A GAUCHE PAR DES ZERO
!          G     CADRAGE A GAUCHE
! OUT CHAINE : CH(*) : CHAINE RECEPTACLE, ON UTILISE TOUTE LA LONGUEUR
!                      DE LA CHAINE
!     ------------------------------------------------------------------
!     REMARQUES :
!      - EN CAS D'ERREUR (A LA TRANSCRIPTION OU DANS LE TYPE DE CADRAGE)
!        LA CHAINE EST REMPLIE D'ETOILE
!      - POUR LES ENTIERS NEGATIFS ==> VALEUR ABSOLUE
!     ------------------------------------------------------------------
!     ROUTINE(S) UTILISEE(S) :
!         -
!     ROUTINE(S) FORTRAN     :
!         LEN    MOD
!     ------------------------------------------------------------------
!
    integer :: lg, ent, ival, base, basmax, il1, il, ier, i
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    parameter   (basmax = 36, base = 36 )
    character(len=1) :: chiffr(0:basmax-1)
    data        chiffr/'0','1','2','3','4','5','6','7','8','9',&
     &                   'A','B','C','D','E','F','G','H','I','J',&
     &                   'K','L','M','N','O','P','Q','R','S','T',&
     &                   'U','V','W','X','Y','Z'/
!
!
    ier = 0
    chaine = ' '
!
    ent = abs(entier)
    lg = len(chaine)
!
!     ON CADRE A DROITE A PRIORI   CADRAGE A DROITE
    il = lg + 1
10  continue
    il = il - 1
    if (il .le. 0) then
        ier = 1
        goto 99000
    else
        ival = mod(ent,base)
        chaine(il:il) = chiffr(ival)
        ent = ent / base
    endif
    if (ent .ne. 0) goto 10
!
!
    if (cadre(1:1) .eq. 'D') then
!        --- CADRAGE A DROITE ---
        if (len(cadre) .gt. 1) then
            if (cadre(2:2) .eq. '0') then
                do 20 i = il-1, 1, -1
                    chaine(i:i) = '0'
20              continue
            endif
        endif
!
    else if (cadre(1:1) .eq. 'G') then
!        --- CADRAGE A GAUCHE ---
        il1 = il-1
        do 30 i = 1, lg-il1
            chaine(i:i) = chaine(i+il1:i+il1)
30      continue
        chaine(lg-il1+1:) = ' '
    else
        ier = 1
    endif
!
!     SORTIE -----------------------------------------------------------
99000  continue
    if (ier .ne. 0) then
        do 9001 i = 1, lg
            chaine(i:i) = '*'
9001      continue
    endif
!
end subroutine
