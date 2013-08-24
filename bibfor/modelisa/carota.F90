subroutine carota(char, noma, irota, ndim)
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
! BUT : STOCKAGE DE LA ROTATION DANS UNE CARTE ALLOUEE SUR LE
!       LIGREL DU MODELE
!
! ARGUMENTS D'ENTREE:
!      CHAR: NOM UTILISATEUR DU RESULTAT DE CHARGE
!      NOMA : NOM DU MAILLAGE
!     IROTA : OCCURENCE DU MOT-CLE FACTEUR ROTATION
!     NDIM : DIMENSION DU PROBLEME
!     LIGRMO : NOM DU LIGREL DE MODELE
!
! ROUTINES APPELEES:
#include "jeveux.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/r8miem.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/char_affe_neum.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mess.h"
#include "asterfort/vetyma.h"
    real(kind=8) :: rota(7), norme
    complex(kind=8) :: cbid
    character(len=8) :: char, noma, licmp(7), k8b, k8tout
    character(len=19) :: carte
    integer :: iocc, irota, nbmail, nbgpma
    integer ::  jncmp, jvalv
    integer :: nbma, ncmp, ndim, nrota, n1, n2, nbtout
    character(len=16) ::  motclf
    integer :: iarg
    character(len=19) :: cartes(1)
    integer :: ncmps(1)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    motclf = 'ROTATION'
    do iocc = 1, irota
!
        call getvr8('ROTATION', 'VITESSE', iocc, iarg, 1,&
                    rota(1), n1)
        call getvr8('ROTATION', 'AXE', iocc, iarg, 3,&
                    rota(2), n2)
        call getvr8('ROTATION', 'CENTRE', iocc, iarg, 3,&
                    rota(5), nrota)
!
        if (n1 .gt. 0) then
            ASSERT(n1.eq.1)
            ASSERT(n2.eq.3)
            norme=sqrt( rota(2)*rota(2)+rota(3)*rota(3)+rota(4)*rota(&
            4) )
            if (norme .gt. r8miem()) then
                rota(2)=rota(2)/norme
                rota(3)=rota(3)/norme
                rota(4)=rota(4)/norme
            else
                call u2mess('F', 'MODELISA3_63')
            endif
        endif
        call getvtx('ROTATION', 'MAILLE', iocc, iarg, 1,&
                    k8b, nbmail)
        call getvtx('ROTATION', 'GROUP_MA', iocc, iarg, 1,&
                    k8b, nbgpma)
        call getvtx('ROTATION', 'TOUT', iocc, iarg, 1,&
                    k8tout, nbtout)
        nbma = nbmail+nbgpma
!
!
!   SI NBMA = 0, ALORS IL N'Y A AUCUN MOT CLE GROUP_MA OU MAILLE ,
!   DONC LA ROTATION S'APPLIQUE A TOUT LE MODELE (VALEUR PAR DEFAUT)
!
        if ((nbma.eq.0) .and. (nbtout.eq.1)) then
!
!   UTILISATION DE LA ROUTINE MECACT (PAS DE CHANGEMENT PAR RAPPORT
!   A LA PRECEDENTE FACON DE PRENDRE EN COMPTE LA PESANTEUR)
!
            carte=char//'.CHME.ROTAT'
            licmp(1)='OME'
            licmp(2)='AR'
            licmp(3)='BR'
            licmp(4)='CR'
            licmp(5)='X'
            licmp(6)='Y'
            licmp(7)='Z'
            call mecact('G', carte, 'MAILLA', noma, 'ROTA_R',&
                        7, licmp, 0, rota, cbid,&
                        ' ')
!
        else if ((nbma.ne.0).and.(k8tout.eq.'NON')) then
!
!   APPLICATION DE LA ROTATION AUX MAILLES OU GROUPES DE MAILLES
!   MENTIONNES. ROUTINE MODIFIEE ET CALQUEE SUR LA PRISE EN COMPTE
!   D'UNE ROTATION (CBROTA ET CAROTA)
!
            carte=char//'.CHME.ROTAT'
            call alcart('G', carte, noma, 'ROTA_R')
            call jeveuo(carte//'.NCMP', 'E', jncmp)
            call jeveuo(carte//'.VALV', 'E', jvalv)
!
! --- STOCKAGE DE FORCES NULLES SUR TOUT LE MAILLAGE
!
            ncmp = 7
            zk8(jncmp)='OME'
            zk8(jncmp+1)='AR'
            zk8(jncmp+2)='BR'
            zk8(jncmp+3)='CR'
            zk8(jncmp+4)='X'
            zk8(jncmp+5)='Y'
            zk8(jncmp+6)='Z'
!
            zr(jvalv) = 0.d0
            zr(jvalv+1) = 0.d0
            zr(jvalv+2) = 0.d0
            zr(jvalv+3) = 0.d0
            zr(jvalv+4) = 0.d0
            zr(jvalv+5) = 0.d0
            zr(jvalv+6) = 0.d0
!
            call nocart(carte, 1, ' ', 'NOM', 0,&
                        ' ', 0, ' ', ncmp)
!
!
! --- STOCKAGE DANS LA CARTE
!
            zr(jvalv) = rota(1)
            zr(jvalv+1) = rota(2)
            zr(jvalv+2) = rota(3)
            zr(jvalv+3) = rota(4)
            zr(jvalv+4) = rota(5)
            zr(jvalv+5) = rota(6)
            zr(jvalv+6) = rota(7)
!
!
            cartes(1) = carte
            ncmps(1) = ncmp
            call char_affe_neum(noma, ndim, motclf, iocc, 1, &
                                cartes, ncmps)

        else if ((nbma.ne.0).and.(k8tout.eq.'OUI')) then
            call u2mess('F', 'MODELISA3_40')
        endif
    end do
end subroutine
