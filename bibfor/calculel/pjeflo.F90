subroutine pjeflo(elrefa, ndim, ipb, xr2, alarm,&
                  ma2, ino2, ma1, ima1, lext)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: alarm, elrefa
    character(len=8) :: ma1, ma2
    integer :: ipb, ima1, ndim, ino2
    real(kind=8) :: xr2(ndim)
    logical :: lext
! ----------------------------------------------------------------------
! BUT :
!   * EMETTRE UNE ALARME SI INO2 EST TROP LOIN DE IMA1.
!   * DETERMINER SI INO2 EST EXTERIEUR A IMA1
! ----------------------------------------------------------------------
!
! IN  ELREFA : ELREFA DE L'ELEMENT
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  XR2     : COORDONNEES DU POINT DANS L'ESPACE PARA DE L'ELEMENT
!              (CALCULE PAR REEREG)
! IN  IPBD   : CODE RETOUR DE REEREG
! IN  ALARM  : 'OUI'/'NON' (VEUT-ON IMPRIMER L'ALARME)
! IN  MA1    : NOM DU MAILLAGE "1"
! IN  MA2    : NOM DU MAILLAGE "2"
! IN  INO2   : NUMERO DU NOEUD DANS MA2
! IN  IMA1   : NUMERO DE LA MAILLE DANS MA1
! OUT LEXT   : .TRUE. <=> INO2 EST EXTERIEUR A IMA1
! ----------------------------------------------------------------------
!
!
    real(kind=8) :: d, tolala, tolext, x, y, z
    character(len=8) :: nomno, nomma
    character(len=24) :: valk(5)
! ----------------------------------------------------------------------
    call assert(alarm.eq.'OUI' .or. alarm.eq.'NON')
    lext=.false.
!
!
!     -- SI REEREG N'A PAS CONVERGE, ON N'A PAS CONFIANCE DANS XR2 :
    if (ipb .ne. 0) then
        d=999.d0
        goto 70
!
    endif
!
    if (ndim .ge. 1) x=xr2(1)
    if (ndim .ge. 2) y=xr2(2)
    if (ndim .ge. 3) z=xr2(3)
!
!
!     -- POUR LES HEXA : KSI,ETA,DZETA SONT DANS [-1,1]
!     ----------------------------------------------------
    if (elrefa .eq. 'HE8' .or. elrefa .eq. 'H20' .or. elrefa .eq. 'H27') then
        call assert(ndim.eq.3)
        if (abs(x) .gt. 1.d0) goto 10
        if (abs(y) .gt. 1.d0) goto 10
        if (abs(z) .gt. 1.d0) goto 10
!
!       -- ON EST INTERIEUR
        goto 80
!
10      continue
!       -- ON EST EXTERIEUR. EST-ON LOIN ?
        d=0.d0
        d=max(d,abs(x)-1.d0)
        d=max(d,abs(y)-1.d0)
        d=max(d,abs(z)-1.d0)
!
!
!     -- POUR LES TETRA :
!     ----------------------------------------------------
    else if (elrefa.eq.'TE4' .or. elrefa.eq.'T10') then
        call assert(ndim.eq.3)
        if (x .lt. 0.d0) goto 20
        if (y .lt. 0.d0) goto 20
        if (z .lt. 0.d0) goto 20
        if (x+y+z .gt. 1.d0) goto 20
!
!       -- ON EST INTERIEUR
        goto 80
!
20      continue
!       -- ON EST EXTERIEUR. EST-ON LOIN ?
        d=0.d0
        d=max(d,-x)
        d=max(d,-y)
        d=max(d,-z)
        d=max(d,x+y+z-1.d0)
!
!
!     -- POUR LES PYRAM :
!     ----------------------------------------------------
    else if (elrefa.eq.'PY5' .or. elrefa.eq.'P13') then
        call assert(ndim.eq.3)
        if (z .lt. 0.d0) goto 30
        if (x+y+z .gt. 1.d0) goto 30
        if (x-y+z .gt. 1.d0) goto 30
        if (-x+y+z .gt. 1.d0) goto 30
        if (-x-y+z .gt. 1.d0) goto 30
!
!       -- ON EST INTERIEUR
        goto 80
!
30      continue
!       -- ON EST EXTERIEUR. EST-ON LOIN ?
        d=0.d0
        d=max(d,-z)
        d=max(d,x+y+z-1.d0)
        d=max(d,x-y+z-1.d0)
        d=max(d,-x+y+z-1.d0)
        d=max(d,-x-y+z-1.d0)
!
!
!     -- POUR LES PENTA :
!     ----------------------------------------------------
        elseif (elrefa.eq.'PE6' .or. elrefa.eq.'P15' .or. elrefa.eq.'P18')&
    then
        call assert(ndim.eq.3)
        if (x .lt. -1.d0) goto 40
        if (x .gt. +1.d0) goto 40
        if (y .lt. 0.d0) goto 40
        if (z .lt. 0.d0) goto 40
        if (y+z .gt. 1.d0) goto 40
!
!       -- ON EST INTERIEUR
        goto 80
!
40      continue
!       -- ON EST EXTERIEUR. EST-ON LOIN ?
        d=0.d0
        d=max(d,abs(x)-1.d0)
        d=max(d,-y)
        d=max(d,-z)
        d=max(d,+y+z-1.d0)
!
!
!     -- POUR LES TRIA :
!     ----------------------------------------------------
        elseif (elrefa.eq.'TR3' .or. elrefa.eq.'TR6' .or. elrefa.eq.'TR7')&
    then
        call assert(ndim.eq.2)
        if (x .lt. 0.d0) goto 50
        if (y .lt. 0.d0) goto 50
        if (x+y .gt. 1.d0) goto 50
!
!       -- ON EST INTERIEUR
        goto 80
!
50      continue
!       -- ON EST EXTERIEUR. EST-ON LOIN ?
        d=0.d0
        d=max(d,-x)
        d=max(d,-y)
        d=max(d,+x+y-1.d0)
!
!
!     -- POUR LES QUAD :
!     ----------------------------------------------------
        elseif (elrefa.eq.'QU4' .or. elrefa.eq.'QU8' .or. elrefa.eq.'QU9')&
    then
        call assert(ndim.eq.2)
        if (x .lt. -1.d0) goto 60
        if (y .lt. -1.d0) goto 60
        if (x .gt. +1.d0) goto 60
        if (y .gt. +1.d0) goto 60
!
!       -- ON EST INTERIEUR
        goto 80
!
60      continue
!       -- ON EST EXTERIEUR. EST-ON LOIN ?
        d=0.d0
        d=max(d,-1.d0-x)
        d=max(d,-1.d0-y)
        d=max(d,x-1.d0)
        d=max(d,y-1.d0)
!
    else
        call assert(.false.)
    endif
!
!
!     -- EST-ON EXTERIEUR ?
!     -------------------------------
70  continue
    tolext=1.d-2
    if (d .gt. tolext) lext=.true.
!
!     -- DOIT-ON EMETTRE UNE ALARME ?
!     -------------------------------
    tolala=1.d-1
    if (d .gt. tolala) then
        if (alarm .eq. 'OUI') then
            call jenuno(jexnum(ma2//'.NOMNOE', ino2), nomno)
            call jenuno(jexnum(ma1//'.NOMMAI', ima1), nomma)
            valk(1)=nomno
            valk(2)=ma2
            valk(3)=nomma
            valk(4)=ma1
            call u2mesk('A', 'CALCULEL5_7', 4, valk)
        endif
    endif
!
!
80  continue
end subroutine
