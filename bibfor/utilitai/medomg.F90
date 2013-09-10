subroutine medomg(result, numord, modele, mate, lischa)
!
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/exixfe.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lisccm.h"
#include "asterfort/liscnv.h"
#include "asterfort/liscom.h"
#include "asterfort/lislec.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/rslesd.h"
    integer :: numord
    character(len=8) :: modele, result
    character(len=24) :: mate
    character(len=19) :: lischa
!
! ----------------------------------------------------------------------
!
!  OPERATEUR CALC_G
!
!  SAISIE ET VERIFICATION DE LA COHERENCE DES DONNEES MECANIQUES
!  DU PROBLEME
!
! ----------------------------------------------------------------------
!
! IN  RESULT : NOM DE LA SD RESULTAT
! IN  NUMORD : NUMERO D'ORDRE DANS SD RESULTAT
! OUT MODELE : NOM DU MODELE
! OUT MATE   : MATERIAU CODE
! OUT LISCHA : LISTE DES CHARGES
!
! ----------------------------------------------------------------------
!
    character(len=8) :: materi, carele
    character(len=16) :: phenom, motfac, nomcmd
    character(len=19) :: lisold
    integer :: iexcit,iret,nbchar
    character(len=1) :: base,codarr
    logical :: lxfem
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    materi = ' '
    modele = ' '
    mate = ' '
    nomcmd = 'CALC_G'
    phenom = 'MECANIQUE'
    motfac = 'EXCIT'
    base   = 'V'
    codarr = 'F'
!
! - SUPPRESSION ANCIENNES LISTE_CHARGES
!
    call detrsd(' ', lischa)
!
! - RECUPERATION MODELE, MATERIAU, CARA_ELEM ET LISCHA DANS SD RESU
!
    call rslesd(result, numord, modele, materi, carele,&
                lisold, iexcit)
!
! - CODAGE DU MATERIAU
!
    if (materi .ne. ' ') call rcmfmc(materi, mate)
!
! - ON PREND LE CHARGEMENT DANS LA SD
!
    if (iexcit .eq. 0) then
        call liscnv(phenom, base, lisold, lischa)
    endif
!
! - ON PREND LE CHARGEMENT DONNE PAR L'UTILISATEUR
!
    if (iexcit .eq. 1) then
        call lislec(motfac, phenom, base, lischa)
    endif
!
! - GLUTE: LEVEE ERREUR FATALE POUR XFEM (VOIR 20956)
!
    call exixfe(modele, iret)
    lxfem = iret.ne.0
    if (lxfem) then
      codarr = 'A'
    else
      codarr = 'F'
    endif
!
    call lisnnb(lischa, nbchar)
!
! - VERIFICATION DE LA COHERENCE DES MODELES
!
    if (nbchar.ne.0) call liscom(modele, codarr,lischa)
!
! - VERIFICATION CHARGE PRISE EN COMPTE DANS CALC_G ?
!
    if (nbchar.ne.0) call lisccm(nomcmd, 'F', lischa)
!
    call jedema()
end subroutine
