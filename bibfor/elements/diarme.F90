subroutine diarme(nbt, neq, icodma, ul, dul,&
                  utl, sim, varim, klv, varip,&
                  kty2, duly)
! ----------------------------------------------------------------------
    implicit none
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvalb.h'
    integer :: nbt, neq, icodma
    real(kind=8) :: ul(neq), dul(neq), utl(neq), sim(neq), varim
    real(kind=8) :: klv(nbt), varip, kty2, duly
! ----------------------------------------------------------------------
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
!     RELATION DE COMPORTEMENT "ARME" (ARMEMENT).
!
! ----------------------------------------------------------------------
!
! IN  : NBT    : NOMBRE DE VALEURS POUR LA DEMI-MATRICE
!       NEQ    : NOMBRE DE DDL DE L'ELEMENT
!       ICODMA : ADRESSE DU MATERIAU CODE
!       UL     : DEPLACEMENT PRECEDENT REPERE LOCAL
!       DUL    : INCREMENT DE DEPLACEMENT REPERE LOCAL
!       UTL    : DEPLACEMENT COURANT REPERE LOCAL
!       SIM    : EFFORTS GENERALISES A L'INSTANT PRECEDENT
!       VARIM  : VARIABLE INTERNE A L'INSTANT PRECEDENT
!
! OUT : KLV    : MATRICE TANGENTE
!       VARIP  : VARIABLE INTERNE REACTUALISEE
!       KTY2   :
!       DULY   :
!
!**************** DECLARATION DES VARIABLES LOCALES ********************
!
!-----------------------------------------------------------------------
    integer :: nbpar, nbre2
    real(kind=8) :: dle, dlp, effoy, fle, flp, rap, uly
    real(kind=8) :: utot, valpar, varmax, zero
!-----------------------------------------------------------------------
    parameter    ( nbre2 = 5 )
    real(kind=8) :: kty, kye, kyp, kyg
    real(kind=8) :: valre2(nbre2)
    integer :: codre2(nbre2), kpg, spt
    character(len=8) :: nompar, nomre2(nbre2), fami, poum
!
    data nomre2 /'KYE','DLE','KYP','DLP','KYG'/
!
! ----------------------------------------------------------------------
! --- DEFINITION DES PARAMETRES
!
    zero = 0.d0
    nbpar = 0
    nompar = ' '
    valpar = 0.d0
    call r8inir(nbre2, zero, valre2, 1)
!
! --- CARACTERISTIQUES DU MATERIAU
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, icodma,&
                ' ', 'ARME', nbpar, nompar, valpar,&
                nbre2, nomre2, valre2, codre2, 1)
!
    kye = valre2(1)
    dle = valre2(2)
    kyp = valre2(3)
    dlp = valre2(4)
    kyg = valre2(5)
!
! --- INITITIALISATIONS
!
    effoy = 0.5d0*abs(sim(8)+sim(2))
    rap = 0.d0
    varmax = dlp-dle
    duly = dul(8)-dul(2)
    uly = ul(8)-ul(2)
    utot = abs(utl(8)-utl(2))
    flp = kye*dle+kyp*varim
    fle = kye*abs(duly)
    if (uly .ne. 0.d0) rap = duly/uly
!
! -*-*-*-*       TEST POUR SAVOIR SI L'ON DECHARGE OU NON      *-*-*-*-*
!
    if (rap .lt. 0.d0 .or. duly .eq. 0.d0) then
!
! ======================================================================
!                         ON DECHARGE
! ======================================================================
!
! ******   TEST POUR DETERMINER LA PENTE POUR LA DECHARGE
!
        if (varim .lt. varmax) then
!
! ***** DECHARGE AVEC PENTE ELASTIQUE
!
            varip = varim
            kty = kye
            kty2 = kye
!
        else
!
! ****** DECHARGE AVEC PENTE ULTIME
!
            varip = varmax
            kty = kyg
            kty2 = kyg
!
        endif
!
    else if (rap.gt.0.d0.or.(uly.eq.0.d0.and.duly.ne.0.d0)) then
!
! ======================================================================
!                          ON CHARGE
! ======================================================================
!
! ****** TEST DE POSITION PAR RAPPORT A LA COURBE ULTIME
!
        if (varim .lt. varmax) then
!
! ****** ON EST SOUS LA COURBE ULTIME
!
! **** TEST DE POSITION PAR RAPPORT A LA COURBE PLASTIQUE
!
            if (effoy .lt. flp .or. varim .eq. 0.d0) then
!
! **** ON EST SOUS LA COURBE PLASTIQUE
!
! ** TEST POUR SAVOIR SI ON RESTE SOUS LA COURBE PLASTIQUE
!
                if ((effoy+fle) .lt. flp) then
!
! ** ON RESTE SOUS LA COURBE PLASTIQUE
!
                    varip = varim
                    kty = kye
                    kty2 = kye
!
                else
!
! ** ON NE RESTE PAS SOUS LA COURBE PLASTIQUE
!
                    if (utot .lt. dlp) then
!  ON REJOINT LA COURBE PLASTIQUE
                        varip = abs(utot-dle)
                        kty = kyp
                        kty2 = (kye*dle+kyp*varip-effoy)/abs(duly)
                    else
!  ON REJOINT LA COURBE ULTIME
                        varip = varmax
                        kty2 = kye*dle - effoy + kyp*varmax + kyg*abs(utot-dlp)
                        kty2 = kty2/abs(duly)
                        kty = kyg
                    endif
!
                endif
!
            else
!
! **** ON EST SUR LA COURBE PLASTIQUE
!
! ** TEST POUR SAVOIR SI ON RESTE SUR LA COURBE PLASTIQUE
!
                if (utot .lt. dlp) then
!
! ** ON RESTE SUR LA COURBE PLASTIQUE
!
                    varip = abs(utot-dle)
                    kty = kyp
                    kty2 = kyp
!
                else
!
! ** ON REJOINT LA COURBE ULTIME
!
                    varip = varmax
                    kty2 = kyp*abs(varmax-varim)+kyg*abs(utot-dlp)
                    kty2 = kty2/abs(duly)
                    kty = kyg
!
                endif
!
            endif
!
        else
!
! ****** ON EST SUR LA COURBE ULTIME
!
            varip = varmax
            kty = kyg
            kty2 = kyg
!
        endif
!
    endif
!
! ======================================================================
!                         MODIFICATIONS FINALES
! ======================================================================
!
    klv(3) = kty
    klv(30) = -kty
    klv(36) = kty
!
! ----------------------------------------------------------------------
!
end subroutine
