subroutine tgverm(option, carcri, compor, nno1, nno2,&
                  nno3, geom, ndim, nddl, deplp,&
                  sdepl, vu, vg, vp, vectu,&
                  svect, ncont, contp, scont, nvari,&
                  varip, svari, matuu, smatr, matsym,&
                  epsilo, epsilp, epsilg, varia, iret)
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
! person_in_charge: sebastien.fayolle at edf.fr
! TOLE CRP_21
! TOLE CRS_1404
    implicit none
    include 'jeveux.h'
    include 'asterc/r8miem.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/mavec.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    include 'blas/dcopy.h'
    logical :: matsym
    character(len=16) :: option, compor(*)
    integer :: iret, nno1, nno2, nno3, ndim
    integer :: vu(3, 27), vg(27), vp(27)
    real(kind=8) :: carcri(*), sdepl(*), scont(*), svect(*), smatr(*), varia(*)
    real(kind=8) :: geom(*), deplp(*), vectu(*), contp(*), matuu(*)
    real(kind=8) :: varip(*), svari(*)
!
! ----------------------------------------------------------------------
! VAR OPTION NOM DE L'OPTION DE CALCUL
!             IN  : CELLE UTILISEE PAR LE TE
!             OUT : 'RAPH_MECA' SI BOUCLE, 'FULL_MECA' SI FIN DE BOUCLE
! IN  CARCRI  : CARCRI(1) = type de matrice tangente
!               0 : ANALYTIQUE, on ne passe pas ici
!               1 : PERTURBATION, on calcule Ktgte (FULL_MECA)
!               2 : VERIFICATION, on calcule Ktgte (FULL_MECA) + Kpertu
!               CARCRI(7) = valeur de la perturbation
! OUT IRET   SI IRET = 0 -> FIN, SINON -> BOUCLE
! ----------------------------------------------------------------------
!
! EXEMPLE D'INSERTION DANS UN TE DE L'OPTION FULL_MECA
!  1000 CONTINUE
!       CALL NIFINT(OPTION,...)
!       CALL TGVERM(OPTION,....., IRET)
!       IF (IRET.NE.0) GOTO 1000
!
! ----------------------------------------------------------------------
    character(len=24) :: matra, matrc
    integer :: ematra, ematrc, exi
    integer :: nddl
    integer :: nvari, ncont
    integer :: i, j, k, l, indi, nvar, init, pos
    real(kind=8) :: v, epsilo, fp, fm, pertu, maxdep, maxgeo, maxpre, maxgon
    real(kind=8) :: matper(nddl*nddl), epsilp, epsilg
    save init,pos
    data matra  /'PYTHON.TANGENT.MATA'/
    data matrc  /'PYTHON.TANGENT.MATC'/
    data init,pos /1,0/
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     Calcul de la matrice TGTE par PERTURBATION
!
    iret=0
    if (abs(carcri(2)) .lt. 0.1d0) then
        goto 9999
    else
! INCOMATIBILITE AVEC LES COMPORTEMENTS QUI UTILISENT PVARIMP
        if (compor(5)(1:7) .eq. 'DEBORST') then
            goto 9999
        endif
    endif
    if (option(1:9) .eq. 'RIGI_MECA') then
        goto 9999
    endif
!
! --  INITIALISATION (PREMIER APPEL)
!
    if (init .eq. 1) then
!       PERTURBATION OU VERIFICATION => FULL_MECA
        if (option .ne. 'FULL_MECA') then
            goto 9999
        endif
!
!       CALCUL de la valeur de la perturbation
!       Ici on est en mecanique seule, les DDL sont
!       seulement des deplacements
!
        maxdep=0.d0
        maxpre=0.d0
        maxgon=0.d0
        maxgeo=0.d0
        do 555 i = 1, nno1
            do 600 j = 1, ndim
                maxdep=max(maxdep,abs(deplp(vu(j,i))))
600          continue
555      continue
        do 601 i = 1, nno2
            maxgon=max(maxgon,abs(deplp(vg(i))))
601      continue
        do 602 i = 1, nno3
            maxpre=max(maxpre,abs(deplp(vp(i))))
602      continue
        do 556 i = 1, nno1*ndim
            maxgeo=max(maxgeo,abs(geom(i)))
556      continue
        pertu=carcri(7)
        if (maxdep .gt. pertu*maxgeo) then
            epsilo=pertu*maxdep
        else
            epsilo=pertu*maxgeo
        endif
        if (epsilo .lt. r8miem()) then
            call u2mess('F', 'ALGORITH11_86')
        endif
        epsilp=pertu*maxpre
        if (epsilp .lt. r8miem()) then
            call u2mess('F', 'ALGORITH11_86')
        endif
        epsilg=pertu*maxgon
        if (epsilg .lt. r8miem()) then
            call u2mess('F', 'ALGORITH11_86')
        endif
!      ARCHIVAGE DES VALEURS DE REFERENCE
!
        call dcopy(nddl, deplp, 1, sdepl, 1)
        call dcopy(ncont, contp, 1, scont, 1)
        call dcopy(nddl, vectu, 1, svect, 1)
        call dcopy(nvari, varip, 1, svari, 1)
!
!       ARCHIVAGE DE LA MATRICE TANGENTE COHERENTE
        if (matsym) then
            k = 0
            do 557 i = 1, nddl
                do 558 j = 1, i
                    v = matuu(k+1)
                    k = k + 1
                    smatr((i-1)*nddl+j) = v
                    smatr((j-1)*nddl+i) = v
558              continue
557          continue
        else
            call dcopy(nddl*nddl, matuu, 1, smatr, 1)
        endif
!
!      PREPARATION DES ITERATIONS
!
        option = 'RAPH_MECA'
        iret = 1
        init = 0
        pos = 0
    endif
!
! -- TRAITEMENT DES VARIATIONS
!
!
!    SAUVEGARDE DE LA FORCE INTERIEURE PERTURBEE
!
    nvar = int((pos+1)/2)
!
    if (nvar .gt. 0) then
        call dcopy(nddl, vectu, 1, varia(1+(pos-1)*nddl), 1)
    endif
!
    pos = pos + 1
    nvar = int((pos+1)/2)
    indi = 1-2*mod(pos,2)
!
    if (nvar .le. nddl) then
        call dcopy(nddl, sdepl, 1, deplp, 1)
        do 700 i = 1, nno1
            do 710 j = 1, ndim
                if (nvar .eq. vu(j,i)) then
                    deplp(nvar) = sdepl(nvar) + indi*epsilo
                    goto 800
                endif
710          continue
700      continue
!
        do 720 i = 1, nno2
            if (nvar .eq. vg(i)) then
                deplp(nvar) = sdepl(nvar) + indi*epsilg
                goto 800
            endif
720      continue
!
        do 730 i = 1, nno3
            if (nvar .eq. vp(i)) then
                deplp(nvar) = sdepl(nvar) + indi*epsilp
                goto 800
            endif
730      continue
!
800      continue
!      INITIALISATION DES CHAMPS 'E'
        call r8inir(ncont, 0.d0, contp, 1)
        call r8inir(nddl, 0.d0, vectu, 1)
        iret=1
        goto 9999
    endif
!
!    CALCUL DE LA MATRICE TANGENTE
!
    do 559 i = 1, nddl
        do 560 j = 1, nddl
            fm = varia((2*j-2)*nddl+i)
            fp = varia((2*j-1)*nddl+i)
            do 910 k = 1, nno1
                do 920 l = 1, ndim
                    if (j .eq. vu(l,k)) then
                        v = (fp-fm)/(2*epsilo)
                        goto 900
                    endif
920              continue
910          continue
            do 930 k = 1, nno2
                if (j .eq. vg(k)) then
                    v = (fp-fm)/(2*epsilg)
                    goto 900
                endif
930          continue
            do 940 k = 1, nno3
                if (j .eq. vp(k)) then
                    v = (fp-fm)/(2*epsilp)
                    goto 900
                endif
940          continue
900          continue
            matper((i-1)*nddl+j) = v
560      continue
559  end do
!
!    MENAGE POUR ARRET DE LA ROUTINE
!
    iret = 0
    init = 1
    option = 'FULL_MECA'
!
!    RETABLISSEMENT DE LA SOLUTION
    call dcopy(nddl, sdepl, 1, deplp, 1)
    call dcopy(nddl, svect, 1, vectu, 1)
    call dcopy(ncont, scont, 1, contp, 1)
    call dcopy(nvari, svari, 1, varip, 1)
!
!     PERTURBATION => SAUVEGARDE DE LA MATRICE CALCULEE PAR
!     DIFFERENCES FINIES COMME MATRICE TANGENTE
!
    if (abs(carcri(2)-1.d0) .lt. 0.1d0) then
        if (matsym) then
            call mavec(matper, nddl, matuu, nddl*(nddl+1)/2)
        else
            call dcopy(nddl*nddl, matper, 1, matuu, 1)
        endif
!
!     VERIFICATION
!
    else if (abs(carcri(2)-2.d0).lt.0.1d0) then
        if (matsym) then
            call mavec(smatr, nddl, matuu, nddl*(nddl+1)/2)
        else
            call dcopy(nddl*nddl, smatr, 1, matuu, 1)
        endif
!
!      CREATION DES OBJETS
!      CE N'EST PAS LA PREMIERE FOIS QU'ON CALCULE LA MATRICE TANGENTE
!      -> ON NE CONSERVE QUE LE DERNIER CALCUL (EN COURS)
        call jeexin(matra, exi)
        if (exi .ne. 0) then
            call jedetr(matra)
            call jedetr(matrc)
        endif
!        ON CONSERVE L'ALLOCATION DYNAMIQUE AU DETRIMENT DE L'ALLOCATION
!        STATIQUE, CAR MATRA ET MATRB SONT UTILIES A L'EXTERIEUR DES
!        ROUTINES ELEMENTAIRES
        call wkvect(matra, 'G V R', nddl*nddl, ematra)
        call wkvect(matrc, 'G V R', nddl*nddl, ematrc)
        call dcopy(nddl*nddl, smatr, 1, zr(ematra), 1)
        call dcopy(nddl*nddl, matper, 1, zr(ematrc), 1)
    endif
!
9999  continue
!
    call jedema()
end subroutine
