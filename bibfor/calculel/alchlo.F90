subroutine alchlo(opt, ligrel, nin, lpain, lchin,&
                  nout, lpaout)
    implicit none
!
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
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterc/indik8.h"
#include "asterfort/dchlmx.h"
#include "asterfort/grdeur.h"
#include "asterfort/mecoe1.h"
#include "asterfort/scalai.h"
#include "asterfort/typele.h"
#include "asterfort/wkvect.h"
    integer :: opt, nin, nout
    character(len=8) :: lpain(nin), lpaout(nout)
    character(len=19) :: ligrel
    character(len=19) :: lchin(nin)
! ----------------------------------------------------------------------
!     ENTREES:
!      OPT : OPTION
!     LIGREL : NOM DE LIGREL
!
!     SORTIES:
!     CREATION DES CHAMPS LOCAUX DE NOMS &&CALCUL.NOMPAR(OPT)
!     LE CHAMP LOCAL EST UNE ZONE MEMOIRE TEMPORAIRE A LA ROUTINE CALCUL
!     QUI CONTIENDRA LES  VALEURS DES CHAMPS "BIEN RANGEES"
!     (POUR LES TE00IJ) DE TOUS LES ELEMENTS D'UN GREL.
!
! ----------------------------------------------------------------------
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
    common /caii05/ianoop,ianote,nbobtr,iaobtr,nbobmx
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
    common /caii08/iel
!-----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: iparg, taille, gd
    integer :: iaoptt, lgco, iaopmo, ilopmo, iaopno, ilopno, iaopds
    integer :: iaoppa, npario, iamloc, ilmloc, iadsgd
    integer :: ianoop, ianote, nbobtr, iaobtr, nbobmx, nparin
    integer :: iel, iparin, iparou, nute
    character(len=24) :: nochl, nochl2
    character(len=8) :: nompar
    character(len=8) :: scal
!
!
    call wkvect('&&CALCUL.IA_CHLOC', 'V V I', 3*npario, iawloc)
    call wkvect('&&CALCUL.IA_CHLO2', 'V V I', 5*npario*nbgr, iawlo2)
    call wkvect('&&CALCUL.TYPE_SCA', 'V V K8', npario, iawtyp)
    nbobtr = nbobtr + 1
    zk24(iaobtr-1+nbobtr) = '&&CALCUL.IA_CHLOC'
    nbobtr = nbobtr + 1
    zk24(iaobtr-1+nbobtr) = '&&CALCUL.IA_CHLO2'
    nbobtr = nbobtr + 1
    zk24(iaobtr-1+nbobtr) = '&&CALCUL.TYPE_SCA'
!
!
!     -- INITIALISATION DE CAII06/IAWLO2:
    do 99 igr = 1, nbgr
        nute=typele(ligrel,igr)
        call mecoe1(opt, nute)
99  end do
!
!
    do 40 iparg = 1, npario
        nompar = zk8(iaoppa-1+iparg)
        nochl = '&&CALCUL.'//nompar
        nochl2= '&&CALCUL.'//nompar//'.EXIS'
        zi(iawloc-1+3*(iparg-1)+1)=-1
        zi(iawloc-1+3*(iparg-1)+2)=-1
!
!       SI LE PARAMETRE N'EST ASSOCIE A AUCUN CHAMP, ON PASSE :
!       --------------------------------------------------------
        iparin = indik8(lpain,nompar,1,nin)
        iparou = indik8(lpaout,nompar,1,nout)
        zi(iawloc-1+3*(iparg-1)+3)=iparin+iparou
        if ((iparin+iparou) .eq. 0) goto 40
!
        gd = grdeur(nompar)
        scal = scalai(gd)
        zk8(iawtyp-1+iparg) = scal
        call dchlmx(opt, ligrel, iparg, nin, lpain,&
                    nout, lpaout, taille)
        if (taille .ne. 0) then
            call wkvect(nochl, 'V V '//scal(1:4), taille, zi(iawloc-1+3* (iparg-1)+1))
            nbobtr = nbobtr + 1
            zk24(iaobtr-1+nbobtr) = nochl
            if (iparin .gt. 0) then
                call wkvect(nochl2, 'V V L', taille, zi(iawloc-1+3*( iparg-1)+2))
                nbobtr = nbobtr + 1
                zk24(iaobtr-1+nbobtr) = nochl2
            endif
        else
            zi(iawloc-1+3*(iparg-1)+1)=-2
        endif
40  end do
!
end subroutine
