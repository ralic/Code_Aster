subroutine capesa(char, noma, ipesa, ndim)
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
    implicit none
! BUT : STOCKAGE DE LA PESANTEUR DANS UNE CARTE ALLOUEE SUR LE
!       LIGREL DU MODELE
!
! ARGUMENTS D'ENTREE:
!      CHAR  : NOM UTILISATEUR DU RESULTAT DE CHARGE
!      NOMA  : NOM DU MAILLAGE
!      IPESA : OCCURENCE DU MOT-CLE FACTEUR PESANTEUR
!      NDIM  : DIMENSIOn DU PROBLEME
!      LIGRMO: NOM DU LIGREL DE MODELE
!
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/alcart.h"
#include "asterfort/char_affe_neum.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mess.h"
#include "asterfort/vetyma.h"
    real(kind=8) :: pesa(4), norme, pes(3)
    complex(kind=8) :: cbid
    character(len=8) :: char, noma, licmp(4)
    character(len=19) :: carte
    integer :: iocc, ipesa, nbmail, nbgpma
    integer :: jncmp, jvalv
    integer :: nbma, ncmp, ndim, npesa
    character(len=8) :: k8b
    character(len=16) :: motclf
    integer :: iarg
    character(len=19) :: cartes(1)
    integer :: ncmps(1)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    motclf = 'PESANTEUR'
    do iocc = 1, ipesa
        call getvr8('PESANTEUR', 'GRAVITE', iocc=iocc, scal=pesa(1), nbret=npesa)
        call getvr8('PESANTEUR', 'DIRECTION', iocc=iocc, nbval=3, vect=pes,&
                    nbret=npesa)
!
        norme=sqrt( pes(1)*pes(1)+pes(2)*pes(2)+pes(3)*pes(3) )
        if (norme .gt. r8miem()) then
            pesa(2)=pes(1)/norme
            pesa(3)=pes(2)/norme
            pesa(4)=pes(3)/norme
        else
            call u2mess('F', 'CHARGES2_53')
        endif
!
        call getvtx('PESANTEUR', 'MAILLE', iocc=iocc, scal=k8b, nbret=nbmail)
        call getvtx('PESANTEUR', 'GROUP_MA', iocc=iocc, scal=k8b, nbret=nbgpma)
        nbma=nbmail+nbgpma
!
!   SI NBMA = 0, ALORS IL N'Y A AUCUN MOT CLE GROUP_MA OU MAILLE,
!   DONC LA PESANTEUR S'APPLIQUE A TOUT LE MODELE (VALEUR PAR DEFAUT)
!
        if (nbma .eq. 0) then
!
!   UTILISATION DE LA ROUTINE MECACT (PAS DE CHANGEMENT PAR RAPPORT
!   A LA PRECEDENTE FACON DE PRENDRE EN COMPTE LA PESANTEUR)
!
            carte=char//'.CHME.PESAN'
            licmp(1)='G'
            licmp(2)='AG'
            licmp(3)='BG'
            licmp(4)='CG'
            call mecact('G', carte, 'MAILLA', noma, 'PESA_R',&
                        4, licmp, 0, pesa, cbid,&
                        ' ')
        else
!
!   APPLICATION DE LA PESANTEUR AUX MAILLES OU GROUPES DE MAILLES
!   MENTIONNES. ROUTINE MODIFIEE ET CALQUEE SUR LA PRISE EN COMPTE
!   D'UNE PRESSION (CBPRES ET CAPRES)
!
            carte=char//'.CHME.PESAN'
            call alcart('G', carte, noma, 'PESA_R')
            call jeveuo(carte//'.NCMP', 'E', jncmp)
            call jeveuo(carte//'.VALV', 'E', jvalv)
!
! --- STOCKAGE DE FORCES NULLES SUR TOUT LE MAILLAGE
!
            ncmp = 4
            zk8(jncmp) = 'G'
            zk8(jncmp+1) = 'AG'
            zk8(jncmp+2) = 'BG'
            zk8(jncmp+3) = 'CG'
!
            zr(jvalv) = 0.d0
            zr(jvalv+1) = 0.d0
            zr(jvalv+2) = 0.d0
            zr(jvalv+3) = 0.d0
            call nocart(carte, 1, ' ', 'NOM', 0,&
                        ' ', 0, ' ', ncmp)
!
!
! --- STOCKAGE DANS LA CARTE
!
            zr(jvalv) = pesa(1)
            zr(jvalv+1) = pesa(2)
            zr(jvalv+2) = pesa(3)
            zr(jvalv+3) = pesa(4)
            cartes(1) = carte
            ncmps(1) = ncmp
            call char_affe_neum(noma, ndim, motclf, iocc, 1,&
                                cartes, ncmps)
        endif
    end do
end subroutine
