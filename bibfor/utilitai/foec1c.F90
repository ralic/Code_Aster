subroutine foec1c(iuni, nomf, vec, nbcoup, verif)
    implicit none
    include 'asterfort/fopro1.h'
    integer :: iuni, nbcoup
    character(len=*) :: nomf, vec(*), verif
!     ------------------------------------------------------------------
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
!     ECRITURE DE NIVEAU 1 (IMPR=1) D'UNE FONCTION: ATTRIBUTS
!     ------------------------------------------------------------------
!     ARGUMENTS D'ENTREE:
!        IUNI  : NUMERO D'UNITE LOGIQUE D'ECRITURE
!        NOMFON: NOM UTILISATEUR DE LA FONCTION
!        VEC   : VECTEUR DES ATTRIBUTS (NOMFON.PROL)
!        NBCOUP: NOMBRE DE COUPLES DE VALEURS
!        VERIF : OPTION UTILISATEUR DE VERIFICATION DE CROISSANCE
!                DES VALEURS DU PARAMETRE
!     ------------------------------------------------------------------
    character(len=8) :: interp, prolgd, tprol(3), nompar, nomres
    character(len=19) :: nomfon
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    data tprol/'CONSTANT','LINEAIRE','EXCLU'/
!
    nomfon = nomf
    nompar=vec(3)
    nomres=vec(4)
    write(iuni,*) ' '
    write(iuni,*) ' FONCT_C  ',nomfon,' : ',nomres,' = F(',nompar,')'
    write(iuni,*) '    DONNEE EN ',nbcoup,' POINTS'
    call fopro1(vec, 0, prolgd, interp)
    write(iuni,*) '    INTERPOLATION ',interp
    do 1 i = 1, 3
        if (prolgd(1:1) .eq. tprol(i)(1:1)) then
            write(iuni,*) '    PROLONGEMENT A GAUCHE : ',tprol(i)
        endif
        if (prolgd(2:2) .eq. tprol(i)(1:1)) then
            write(iuni,*) '    PROLONGEMENT A DROITE : ',tprol(i)
        endif
 1  end do
    if (verif .eq. '        ') then
        write(iuni,*) '    LES PARAMETRES DE LA FONCTION SONT REORDONNES'
    else if (verif.eq.'CROISSANT') then
        write(iuni,*) '    VERIFICATION QUE LES PARAMETRES SONT',verif
    endif
end subroutine
