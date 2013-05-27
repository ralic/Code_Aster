subroutine vpdich(lraide, lmasse, ldynam, tol, mxdich,&
                  mxfreq, nfreq, valp, ieme, det,&
                  idet, nbpas, typres, nblagr, solveu)
    implicit none
    include 'asterfort/vpstur.h'
    integer :: lraide, lmasse, ldynam
    real(kind=8) :: tol
    integer :: mxfreq, nfreq, ieme(*), idet(*), nbpas(*), nblagr
    real(kind=8) :: valp(*), det(*)
    character(len=16) :: typres
    character(len=19) :: solveu
!     ------------------------------------------------------------------
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
!     RECHERCHE DE VALEURS PROPRES PAR DICHOTOMIE
!     ------------------------------------------------------------------
! IN  LRAIDE : IS : DESCRIPTEUR DE LA MATRICE DE RAIDEUR
! IN  LMASSE : IS : DESCRIPTEUR DE LA MATRICE DE MASSE
! IN  TOL    : R8 : TOLERANCE D'ADMISSION POUR SEPARATION PAR DICHOTOMIE
! IN  MXFREQ : IS : NOMBRE MAXIMUM DE FREQUENCES A SEPARER
! OUT NFREQ  : IS : NOMBRE DE VALEURS DANS LES TABLEAUX RESULATS
! OUT VALP   : R8 : TABLEAU DES BORNES  DES INTERVALLES (VALEUR PROPRE)
! OUT IEME   : IS : TABLEAU DES POSITIONS MODALES DE LA FREQUENCE
! OUT DET    : R8 :  )   DET * 10**IDET VALEUR DU DETERMINANT DE LA
! OUT IDET   : IS :  )   MATRICE SHIFTEE A LA FREQUENCE I
! OUT NBPAS  : IS : NOMBRE DE DICHOTOMIES EFFECTUEES
! IN  SOLVEU : K19 : SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
!     ------------------------------------------------------------------
!      A UN PAS DONNE ON CALCULE F(I+1/2) = (F(I)+F(I+1))/2
!
!      SI F(I+1/2) EST A LA MEME POSITION MODALE QUE F(I-1)
!         ALORS ON REMPLACE F(I) PAR F(I+1/2)
!      SINONSI F(I+1/2) EST A LA MEME POSITION MODALE QUE F(I+2)
!         ALORS ON REMPLACE F(I+1) PAR F(I+1/2)
!      SINON
!         ON INSERE F(I+1/2) ENTRE F(I) ET F(I+1)
!      FINSI
!
!      CET ALGORITHME AVEC RECUPERATION DE LA PLACE NECESSITE DES
!      TABLEAUX DE TRAVAIL  DE LONGUEUR  MAXIMUM 2*NFREQB + NFREQ
!      OU  NFREQB EST LE NOMBRE DE FREQUENCES DANS LA BANDE DE RECHERCHE
!          NFREQ  EST LE NOMBRE DE FREQUENCES DONNEES MARQUANT LA BANDE
!     ------------------------------------------------------------------
!
    real(kind=8) :: dx, valpx
    integer :: idx, ix
    real(kind=8) :: freq1, freq2
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iborn1, iborn2, ieme0, ieme1, ieme2, ieme3
    integer :: iencor, ier, interv, ip, ipas, iplace, jdec
    integer :: mxdich
!-----------------------------------------------------------------------
    do 10 i = 2, nfreq-1
! --- POUR OPTIMISER ON NE GARDE PAS LA FACTO (SI MUMPS)
        call vpstur(lraide, valp(i), lmasse, ldynam, det(i),&
                    idet(i), ieme(i), ier, solveu, .true.,&
                    .false.)
        ieme(i) = ieme(i) - nblagr
        if (typres .ne. 'DYNAMIQUE') then
            if (valp(i) .lt. 0.d0) then
                ieme(i) = - ieme(i)
            endif
        endif
10  end do
!
    do 100 ipas = 2, mxdich
        ieme0 = - 1
        iencor = 0
        interv = nfreq -1
        iborn1 = 0
        do 110 ip = 1, interv
            iborn1 = iborn1 + 1
            iborn2 = iborn1 + 1
            ieme0 = ieme(max(iborn1-1,1))
            ieme1 = ieme(iborn1)
            ieme2 = ieme(iborn2)
            ieme3 = ieme(min(iborn2+1,nfreq))
!
            if (abs(ieme1-ieme(1)) .le. mxfreq) then
                if (abs(ieme2-ieme1) .gt. 1) then
!
                    freq1 = valp(iborn1)
                    freq2 = valp(iborn2)
                    valpx = (freq1+freq2) * 0.5d0
                    if (abs(freq2-freq1) .ge. tol*valpx) then
                        iencor = 1
                        idx=0
! --- POUR OPTIMISER ON NE GARDE PAS LA FACTO (SI MUMPS)
                        call vpstur(lraide, valpx, lmasse, ldynam, dx,&
                                    idx, ix, ier, solveu, .true.,&
                                    .false.)
                        ix = ix-nblagr
                        if (typres .ne. 'DYNAMIQUE') then
                            if (valpx .lt. 0.d0) then
                                ix = - ix
                            endif
                        endif
                        if (ix .eq. ieme0) then
!                       --- ECRASER LA BORNE INF ---
                            iplace = iborn1
                        else if (ix .eq. ieme3) then
!                       --- ECRASER LA BORNE SUP ---
                            iplace = iborn2
                        else
!                       --- INSERER ENTRE LES DEUX BORNES ---
                            nfreq = nfreq + 1
                            iborn1 = iborn1 + 1
                            do 120 jdec = nfreq, iborn2, -1
                                det( jdec) = det (jdec-1)
                                idet( jdec) = idet (jdec-1)
                                valp( jdec) = valp (jdec-1)
                                ieme( jdec) = ieme (jdec-1)
                                nbpas(jdec) = nbpas(jdec-1)
120                          continue
                            iplace = iborn2
                        endif
!
                        det (iplace) = dx
                        idet (iplace) = idx
                        valp (iplace) = valpx
                        ieme (iplace) = ix
                        nbpas(iplace) = ipas
!
                    endif
                endif
            else
                nfreq = nfreq - (interv - ip + 1)
                goto 100
            endif
!
110      continue
        if (iencor .eq. 0) goto 9999
100  end do
!
9999  continue
!
end subroutine
