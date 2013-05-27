subroutine xsseno(nno, nbsig, nse, npg, jgano,&
                  jsigpg, siseno)
    implicit none
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
!
!    - FONCTIONS REALISEES :  ROUTINE X-FEM
!
!     CALCUL DES CONTRAINTES PAR SOUS-ELEMENTS AUX NOEUDS (SENO)
!
! ......................................................................
!
!
!
!
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/ppgan2.h'
    integer :: mxval
    parameter (mxval=6*3*4)
!     EN 2D :
!     MXVAL =  6 (NBSE MAX) * 3 (NBNOSE MAX) * 4 (NBCMP MAX)
!     EN 3D :
!     MXVAL = 32 (NBSE MAX) * 4 (NBNOSE MAX) * 6 (NBCMP MAX)
!
    integer :: nno, npg, jgano
    integer :: nbsig
    integer :: jsigpg
    integer :: idecpg
    integer :: nse, ise, in, kpg, ic
!
    real(kind=8) :: vpg(15), vno(27)
!
    real(kind=8) :: siseno(mxval)
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!-----------------------------------------------------------------------
!     CALCUL DES CONTRAINTES PAR SOUS-ELEMENTS AUX NOEUDS (SENO)
!-----------------------------------------------------------------------
!
!     BOUCLE SUR LES NSE SOUS-ELEMENTS
    do 110 ise = 1, nse
!
!       DEBUT DE LA ZONE MEMOIRE DE SIG  CORRESPONDANTE
        idecpg=npg*(ise-1)
!
!       BOUCLE NCMP DES CONTRAINTES
        do 120 ic = 1, nbsig
!
            do 121 kpg = 1, npg
                vpg(kpg) = zr(jsigpg+(kpg-1+idecpg)*nbsig+ic-1)
121          continue
!
            call ppgan2(jgano, 1, 1, vpg, vno)
!
            do 122 in = 1, nno
                siseno(nbsig*nno*(ise-1)+nbsig*(in-1)+ic)=vno(in)
122          continue
!
120      continue
!
110  end do
!
    call jedema()
!
end subroutine
