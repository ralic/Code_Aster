subroutine vpstur(lmatk, valshi, lmatm, lmatsh, mantis,&
                  expo, pivot, ier, solveu, caldet,&
                  calfac)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/freqom.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mtdete.h'
    include 'asterfort/preres.h'
    include 'asterfort/u2mesr.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vpshif.h'
    real(kind=8) :: valshi, mantis
    integer :: lmatk, lmatm, lmatsh, expo, pivot, ier
    character(len=19) :: solveu
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     EFFECTUE L'OPERATION DE STURM
!        1) COMBINAISON LINEAIRE MSH =  K - W * M    (W ETANT LE SHIFT)
!        2) DECOMPOSITION DE  MSH
!        3) NOMBRE DE PIVOT NEGATIF
!        4) CALCUL DU DETERMINANT
!     ------------------------------------------------------------------
! IN  VALSHI : R8 : VALEUR DU DECALAGE
! IN  LMATK  : IS : ADRESSE ATTRIBUT MATRICE K
! IN  LMATM  : IS : ADRESSE ATTRIBUT MATRICE M
! IN  LMATSH : IS : ADRESSE ATTRIBUT MATRICE SHIFTEE
! OUT MANTIS : R8 : MANTISSE DU DETERMINANT
! OUT EXPO   : IS : EXPOSANT DU DETERMINANT
! OUT PIVOT  : IS : NOMBRE DE TERMES DIAGONAUX NEGATIFS
! OUT IER    : IS : CODE RETOUR  /= 0 ==> LE SHIFT EST UNE VALEUR PROPRE
! IN  SOLVEU : K19 : SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
! IN  CALDET : LOG : SI TRUE ON CALCULE LE DETERMINANT, SI FALSE ON NE
!                     LE CALCULE PAS (GAIN TEMPS)
! IN  CALFAC : LOG : SI MUMPS: SI TRUE, ON GARDE LES TERMES DE LA FACTO
!                    RISEE, SI FALSE, ON NE LES GARDE PAS (GAIN ESPACE)
!     ------------------------------------------------------------------
!
!
    integer :: iret, npvneg, islvk, islvi, iold, iold2
    real(kind=8) :: valr
    complex(kind=8) :: cbid
    character(len=19) :: matpre, matass
    character(len=24) :: metres
    logical :: caldet, calfac
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
!
!     --- INITIALISATION ---
    call jeveuo(solveu//'.SLVK', 'L', islvk)
    metres=zk24(islvk)
    call jeveuo(solveu//'.SLVI', 'E', islvi)
!
!
! ---- OPTIMISATION MUMPS VIA CALFAC/CALDET: PART 1/2
! ---- SI CALFAC=.FALSE., ON NE STOCKE PAS LES FACTEURS, SEUL LE
! ---- CARACTERE SINGULIER ET LE NBRE DE TERMES <0 DE LA DIAGONALE NOUS
! ---- INTERESSENT
! ---- SI CALDET=.TRUE.: A PARTIR DE MUMPS.4.10.0 ON CALCULE LE DET
    iold=-9999
    iold2=-9999
    if (metres(1:5) .eq. 'MUMPS') then
        if (.not.calfac) then
            iold=zi(islvi-1+4)
            zi(islvi-1+4)=1
        endif
        if (caldet) then
            iold2=zi(islvi-1+5)
            zi(islvi-1+5)=1
        endif
    endif
!
!     --- DECALAGE SPECTRAL  K - W * M    (W ETANT LE SHIFT) ---
    call vpshif(lmatk, valshi, lmatm, lmatsh)
!
!     --- FACTORISATION LDLT DE LA MATRICE SHIFTEE---
    ier=0
!
    matpre=' '
    matass=zk24(zi(lmatsh+1))
    call preres(solveu, 'V', iret, matpre, matass,&
                npvneg, 2)
!
    if (iret .ge. 1) ier = 1
    if (iret .gt. 1) then
        valr = freqom(valshi)
        call u2mesr('A', 'ALGELINE5_27', 1, valr)
    endif
    pivot = - npvneg
!
! ---  CALCUL OPTIONNEL DU DETERMINANT
! ---- OPTIMISATION VIA CALDET (SI MF OU LDLT OU MUMPS V4.10.0 ET PLUS)
    if (caldet) then
        if ((metres(1:10).ne.'MULT_FRONT') .and. (metres(1:4) .ne.'LDLT') .and.&
            (metres(1:5).ne.'MUMPS')) then
            call u2mess('F', 'ALGELINE5_73')
        else
            call mtdete(1, metres, lmatsh, mantis, expo,&
                        cbid)
        endif
    endif
!
! ---- OPTIMISATION MUMPSVIA CALFAC/CALDET: PART 2/2
! ---- ON REMET DANS LA SD_SOLVEUR.SLVI(4) L'ANCIENNE VALEUR
! ---- (NORMALEMENT LA VALEUR INITIALISEE PAR DEFAUT -9999).
    if (metres(1:5) .eq. 'MUMPS') then
        if (.not.calfac) zi(islvi-1+4)=iold
        if (caldet) zi(islvi-1+5)=iold2
    endif
!
    call jedema()
end subroutine
