subroutine ecrbas(nbsauv, nbnl, nbmode, depgen, vitgen,&
                  accgen, temps, jordre, ptemp, depbut,&
                  vitbut, forbut, redepg, revitg, reaccg,&
                  retemp, reordr, reptem, redepb, revitb,&
                  reforb)
! aslint: disable=W1504
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! DESCRIPTION : ARCHIVAGE DES RESULTATS
! -----------
!               APPELANT : MDITM2
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: nbsauv, nbnl, nbmode
    real(kind=8) :: depgen(nbmode, *), vitgen(nbmode, *), accgen(nbmode, *)
    real(kind=8) :: temps(*), ptemp(*)
    integer :: jordre(*)
    real(kind=8) :: depbut(nbnl, 3, *), vitbut(nbnl, 3, *), forbut(nbnl, 3, *)
    real(kind=8) :: redepg(*), revitg(*), reaccg(*), retemp(*), reptem(*)
    integer :: reordr(*)
    real(kind=8) :: redepb(*), revitb(*), reforb(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, ideck, ideckj, j, k
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!-----------------------------------------------------------------------
! 1.  DISCRETISATION TEMPORELLE
!-----------------------------------------------------------------------
!
! 1.1 INSTANTS
!
    do 110 k = 1, nbsauv
        retemp(k) = temps(k)
110  end do
!
! 1.2 NUMEROS D'ORDRE DES INSTANTS
!
    do 120 k = 1, nbsauv
        reordr(k) = jordre(k)
120  end do
!
! 1.3 PAS DE TEMPS
!
    do 130 k = 1, nbsauv
        reptem(k) = ptemp(k)
130  end do
!
!-----------------------------------------------------------------------
! 2.  GRANDEURS GENERALISEES
!-----------------------------------------------------------------------
!
! 2.1 DEPLACEMENTS GENERALISES
!
    do 210 k = 1, nbsauv
        ideck = (k-1)*nbmode
        do 211 i = 1, nbmode
            redepg(ideck + i) = depgen(i,k)
211      continue
210  end do
!
! 2.2 VITESSES GENERALISEES
!
    do 220 k = 1, nbsauv
        ideck = (k-1)*nbmode
        do 221 i = 1, nbmode
            revitg(ideck + i) = vitgen(i,k)
221      continue
220  end do
!
! 2.3 ACCELERATIONS GENERALISEES
!
    do 230 k = 1, nbsauv
        ideck = (k-1)*nbmode
        do 231 i = 1, nbmode
            reaccg(ideck + i) = accgen(i,k)
231      continue
230  end do
!
!-----------------------------------------------------------------------
! 3.  GRANDEURS PHYSIQUES AUX NOEUDS DE CHOC
!-----------------------------------------------------------------------
!
    if (nbnl .ne. 0) then
!
! 3.1    FORCES DE CONTACT
!
        do 310 k = 1, nbsauv
            ideck = (k-1)*3*nbnl
            do 311 j = 1, nbnl
                ideckj = ideck + (j-1)*3
                do 312 i = 1, 3
                    reforb(ideckj + i) = forbut(j,i,k)
312              continue
311          continue
310      continue
!
! 3.2    DEPLACEMENTS DES NOEUDS DE CHOC
!
        do 320 k = 1, nbsauv
            ideck = (k-1)*3*nbnl
            do 321 j = 1, nbnl
                ideckj = ideck + (j-1)*3
                do 322 i = 1, 3
                    redepb(ideckj + i) = depbut(j,i,k)
322              continue
321          continue
320      continue
!
! 3.3    VITESSES DES NOEUDS DE CHOC
!
        do 330 k = 1, nbsauv
            ideck = (k-1)*3*nbnl
            do 331 j = 1, nbnl
                ideckj = ideck + (j-1)*3
                do 332 i = 1, 3
                    revitb(ideckj + i) = vitbut(j,i,k)
332              continue
331          continue
330      continue
!
    endif
!
! --- FIN DE ECRBAS.
end subroutine
