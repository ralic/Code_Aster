subroutine sndbg(ifm, iclass, ival, rval, kval)
    implicit none
    integer :: ifm, iclass, ival
    real(kind=8) :: rval(*)
    character(len=*) :: kval
!     ------------------------------------------------------------------
!          ECRITURE DE CE QUE L'ON A TROUVE (NIVEAU SN)
!     ------------------------------------------------------------------
! IN  ICLASS   CLASSE  DE CE QUE L'ON A TROUVE
!     ------------------------------------------------------------------
!           -- TYPE -----    ---- INFORMATION --------------------------
!      -1   FIN DE FICHIER
!       0   ERREUR           CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
!       1   ENTIER           IVAL DE TYPE INTEGER
!       2   REEL             RVAL(1) DE TYPE REAL*8
!       3   IDENTIFICATEUR   CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
!       4   TEXTE            CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
!       5   COMPLEXE         RVAL(1),RVAL(2)
!       6   BOOLEEN          IVAL  = 1 = VRAI , 0 = FAUX
!
!           SEPARATEUR       CVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
!       7   '('
!       8   ')'
!       9   ','
!      10   ':'
!      11   '='
!      12   ';'
!      13   SEPARATEUR INDEFINI
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     ROUTINE(S) UTILISEE(S) :
!         -
!     ROUTINE(S) FORTRAN     :
!         WRITE
!     ------------------------------------------------------------------
! FIN SNDBG
!     ------------------------------------------------------------------
!
    character(len=12) :: pgm
    character(len=80) :: cval
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    cval = kval
    pgm = ' <SNDBG >:  '
    write(ifm,'(1X,72(''-''))')
    if (iclass .eq. -1) then
        write(ifm,*) pgm,'EOF    : "FIN DE FICHIER"'
    else if (iclass.eq.0) then
        write(ifm,*) pgm,'ERREUR : "'//cval(:ival)//'"'
    else if (iclass.eq.1) then
        write(ifm,*) pgm,'ENTIER :', ival
    else if (iclass.eq.2) then
        write(ifm,*) pgm,'REEL   :', rval(1)
    else if (iclass.eq.3) then
        write(ifm,*) pgm,'IDENT  : "'//cval(:ival)//'"'
    else if (iclass.eq.4) then
        write(ifm,*) pgm,'TEXTE  : "'//cval(:ival)//'"'
    else if (iclass.eq.5) then
        write(ifm,*) pgm,'CMPLX  : (',rval(1),',',rval(2),')'
    else if (iclass.eq.6) then
        write(ifm,*) pgm,'BOOLEAN: ',ival
    else if (iclass.gt.6.and.iclass.lt.13) then
        write(ifm,*) pgm,iclass,'  : "'//cval(:ival)//'"'
    else if (iclass.eq.13) then
        write(ifm,*) pgm,'UNDEF  : "'//cval(:ival)//'"'
    else
        write(ifm,*) pgm,'CLASSE INDEFINIE ',iclass
    endif
end subroutine
