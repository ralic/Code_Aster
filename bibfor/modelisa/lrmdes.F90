subroutine lrmdes(fid, nbltit, descfi, titre)
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
! person_in_charge: nicolas.sellenet at edf.fr
!-----------------------------------------------------------------------
!     LECTURE FORMAT MED - LA DESCRIPTION
!     -    -         -        ---
!-----------------------------------------------------------------------
!     LECTURE DU FICHIER MAILLAGE AU FORMAT MED
!               PHASE 0 : LA DESCRIPTION
!     ENTREES :
!       FID    : IDENTIFIANT DU FICHIER MED
!     SORTIES:
!       NBLTIT : NOMBRE DE LIGNES DU TITRE
!       DESCFI : DESCRIPTION DU FICHIER
!       TITRE  : TITRE DU MAILLAGE
!-----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'jeveux.h'
    include 'asterfort/enlird.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/mffien.h'
    include 'asterfort/wkvect.h'
    integer :: fid
    integer :: nbltit
!
    character(len=*) :: descfi, titre
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
!
    integer :: codret
    integer :: jtitre
!
    character(len=80) :: dat
!
!     ------------------------------------------------------------------
    call jemarq()
!
!====
! 1. LECTURE DE LA DESCRIPTION EVENTUELLE DU FICHIER
!====
!
    descfi=' '
!
    call mffien(fid, descfi, codret)
!     POUR CERTAINES ROUTINES MED CODRET = -1 N'EST PAS UN PROBLEME
!      IF ( CODRET.NE.0 ) THEN
!        SAUX08='MFFIEN  '
!        CALL U2MESG('F','DVP_97',1,SAUX08,1,CODRET,0,0.D0)
!      ENDIF
!
!====
! 2. OBJET TITRE
!    ON Y MET LA DESCRIPTION SI ELLE EXISTE, LA DATE SINON.
!====
!
    nbltit = 1
    call wkvect(titre, 'G V K80', nbltit, jtitre)
!
    if (descfi .ne. ' ') then
        zk80(jtitre) = descfi
    else
        call enlird(dat)
        zk80(jtitre) = dat
    endif
!
!====
! 3. LA FIN
!====
!
    call jedema()
!
end subroutine
