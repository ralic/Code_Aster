subroutine fstat0(nbpt, fn, offset, fnmoyt, fnmoyc,&
                  fnrmst, fnrmsc, fnmax, fnmin, fmaxmo,&
                  fminmo, nbmaxr, nbminr)
!***********************************************************************
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
! CETTE ROUTINE EST EN FAIT L'ANCIENNE FSTAT RENOMMEE FSTAT0
!
!       MOYENNAGE STATISTIQUE DES FORCES AMV
!
!
!
    implicit none
    real(kind=8) :: fn(*), fnmoyt, fnmoyc, fnrmsc, fnrmst, fnmax, fnmin
    real(kind=8) :: sminr, smaxr, offset, fmaxmo, fminmo, sfn2, sfn
!
!
!       ARGUMENTS:
!       ----------------------------------------
!       IN:
!            NBPT         NB DE POINTS DU TABLEAU A ANALYSER
!            FN           TABLEAU A ANALYSER
!            OFFSET       VALEUR DE SEUIL DE DETECTION DES VALEURS
!
!       OUT:
!            FNMOY        VALEUR MOYENNE ( COMPTAGE AU DESSUS DU SEUIL )
!            FNETYP       ECART TYPE ( VALEUR DITE PARFOIS RMS )
!            FNRMS        SQRT DE LA MOYENNE DES CARRES ( RMS POUR DES F
!                         REDRESSEES )
!            FNMAX        VALEUR MAXIMUM ABSOLU DU TABLEAU
!            FNMIN        VALEUR MINIMUM ABSOLU DE LA FONCTION
!            FMAXMO       MOYENNE DES MAXIMAS RELATIFS DE LA FONCTION
!            FMINMO       MOYENNE DES MINIMAS RELATIFS DE LA FONCTION
!
!
!
!       VARIABLES UTILISEES
!       ----------------------------------------
!       SFN SOMME DES FORCES DE CHOC
!       SFN2 SOMME DES CARRES DES FORCES DE CHOC
!       NBCOUNT NOMBRE DE VALEURS DU TABLEAU > SEUIL
!       NBMAXR  NOMBRE DE MAXIMAS RELATIFS RENCONTRES
!       NBMINR  NOMBRE DE MINIMAS RELATIFS RENCONTRES
!       SMAXR   SOMME DES MAXIMAS RELATIFS
!       SMINR   SOMME DES MINIMAS RELATIFS
!
!-----------------------------------------------------------------------
    integer :: i, nbmaxr, nbminr, nbpt, ncount
!-----------------------------------------------------------------------
    sfn = 0.d0
    sfn2 = 0.d0
    fnmax = -10.d20
    fnmin = -fnmax
    ncount = 0
    nbminr = 0
    nbmaxr = 0
    smaxr = 0.0d0
    sminr = 0.0d0
!
    do 10 i = 1, nbpt
!
        if ((abs(fn(i))) .gt. offset) then
            ncount = ncount + 1
            sfn = sfn + fn(i)
!
!           RECHERCHE DES EXTREMAS ABSOLUS
!
            if (fn(i) .gt. fnmax) fnmax = fn(i)
            if (fn(i) .lt. fnmin) fnmin = fn(i)
!
!           RECHERCHE DES EXTREMAS RELATIFS
!
            if ((i.gt.1) .and. (i.lt.nbpt)) then
!
                if ((fn(i).gt.fn(i-1)) .and. (fn(i).gt.fn(i+1))) then
                    smaxr = smaxr + fn(i)
                    nbmaxr = nbmaxr + 1
                endif
!
                if ((fn(i).lt.fn(i-1)) .and. (fn(i).lt.fn(i+1))) then
                    sminr = sminr + fn(i)
                    nbminr = nbminr + 1
                endif
!
            endif
!
        endif
!
10  end do
!
    if (ncount .ne. 0) then
        fnmoyc = sfn/dble(ncount)
        fnmoyt = sfn/dble(nbpt)
!
    else
        fnmoyc = 0.d0
        fnmoyt = sfn/dble(nbpt)
    endif
!
    if (nbminr .ne. 0) then
        fminmo = sminr/dble(nbminr)
!
    else
        fminmo = 0.d0
    endif
!
    if (nbmaxr .ne. 0) then
        fmaxmo = smaxr/dble(nbmaxr)
!
    else
        fmaxmo = 0.d0
    endif
!
    do 20 i = 1, nbpt
        if (abs(fn(i)) .gt. offset) then
            sfn2 = sfn2 + fn(i)**2
        endif
!
20  end do
!
    if (ncount .ne. 0) then
        fnrmsc = sqrt(sfn2/dble(ncount))
        fnrmst = sqrt(sfn2/dble(nbpt))
!
    else
        fnrmsc = 0.d0
        fnrmst = sqrt(sfn2/dble(nbpt))
!
        fnmin = 0.d0
        fnmax = 0.d0
    endif
!
    goto 30
!
30  continue
end subroutine
