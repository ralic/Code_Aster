subroutine chtpcn(chno1, tgeom, tailmi, tmin, epsi,&
                  base, chno2)
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
#include "jeveux.h"
#include "asterfort/antece.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    character(len=*) :: chno1, base, chno2
    real(kind=8) :: tgeom(6), tmin, epsi, tailmi, val
!
!----------------------------------------------------------------------
! AUTEUR: G.ROUSSEAU
!
! BUT:
! ----
! TRANSPORTER UN CHAMNO DE TEMP_R DEFINI SUR UNE PARTIE
! DU MAILLAGE D INTERFACE SUR UNE AUTRE PARTIE DE L INTERFACE
! CORRESPONDANT AUX CONTOURS IMMERGES D UNE
! SOUS-STRUCTURE NON MAILLEES PAR UNE TRANSFORMATION GEOMETRIQUE
!----------------------------------------------------------------------
!
! ARGUMENTS:
! ----------
! IN/JXIN  CHNO1: K19 : CHAM_NO DONT ON VA RECUPERER LES VALEURS
! IN       BASE   : K1  : NOM DE LA BASE SUR LAQUELLE LE CHAM_NO DOIT
!                         ETRE CREE
! IN       TAILMI  : R   : TAILLE DE MAILLE MIN
! IN       TGEOM : L_R8: TABLE DES COMPOSANTES DE LA TRANSFORMATION
!                        GEOMETRIQUE
!          3 COMPOSANTES DE TRANSL PUIS 3 ANGLES NAUTIQUES
!          DE ROTATION
! IN       TMIN : R8 : TEMP MINIMALE EN DECA DE LAQUELLE ON PEUT
!          AFFECTER
!          AU NOEUD UNE VALEUR DU CHAMNO A TRANSPORTER
!
! IN/JXOUT CHNO2: K19 : NOM DU CHAM_NO A CREER
!
!-----------------------------------------------------------------------
!
!
!
!
!
!
    integer :: nbante, ino1
    character(len=8) :: gd1, repk, ma
    character(len=24) :: valk(2)
    character(len=8) :: diff, chnaff
    character(len=19) :: cn1, cn2, pchno1, pchno2
!
!-----------------------------------------------------------------------
    integer :: iaval1, iaval2, ibid, ieq1, ieq2, ierd, ino2
    integer :: inueq1, inueq2, iprn1, iprn2, ival1, ival2, nbcn1
    integer :: nbnaff, nbno, nbnrcp, ncmp1, ncmp2, nec
!-----------------------------------------------------------------------
    call jemarq()
    cn1 = chno1
    cn2 = chno2
    call copisd('CHAMP_GD', base, cn1, cn2)
!
!
! ------------------------------ VERIFICATIONS -------------------------
!
    call dismoi('F', 'NOM_GD', cn1, 'CHAM_NO', ibid,&
                gd1, ierd)
    call dismoi('F', 'PROF_CHNO', cn1, 'CHAM_NO', ibid,&
                pchno1, ierd)
    call dismoi('F', 'PROF_CHNO', cn2, 'CHAM_NO', ibid,&
                pchno2, ierd)
!
!
!
!
    call jeveuo(cn1//'.VALE', 'L', iaval1)
    call jelira(cn1//'.VALE', 'LONMAX', nbcn1)
!
!
    call jeveuo(cn2//'.VALE', 'E', iaval2)
!
!
    call jenonu(jexnom(pchno1//'.LILI', '&MAILLA'), ibid)
    call jeveuo(jexnum(pchno1//'.PRNO', ibid), 'L', iprn1)
    call jenonu(jexnom(pchno2//'.LILI', '&MAILLA'), ibid)
    call jeveuo(jexnum(pchno2//'.PRNO', ibid), 'L', iprn2)
    call jeveuo(pchno1//'.NUEQ', 'L', inueq1)
    call jeveuo(pchno2//'.NUEQ', 'L', inueq2)
!
    call dismoi('F', 'NOM_MAILLA', cn1, 'CHAM_NO', ibid,&
                ma, ierd)
    call dismoi('F', 'NB_NO_MAILLA', ma, 'MAILLAGE', nbno,&
                repk, ierd)
!
    call dismoi('F', 'NB_EC', gd1, 'GRANDEUR', nec,&
                repk, ierd)
!
! NOMBRE DE NOEUDS A AFFECTER
!
    nbnaff = 0
    do 10, ino1 =1,nbno
    ncmp1= zi(iprn1-1+ (ino1-1)* (nec+2)+2)
    if (ncmp1 .eq. 0) goto 10
    ival1 = zi(iprn1-1+ (ino1-1)* (nec+2)+1)
    ieq1 = zi(inueq1-1+ival1-1+1)
    val = zr(iaval1-1+ieq1)
    if (abs(val) .lt. tmin) nbnaff=nbnaff+1
    10 end do
!
!
!
    nbnrcp = 0
    do 1, ino2=1,nbno
!
    ival2 = zi(iprn2-1+ (ino2-1)* (nec+2)+1)
    ncmp2 = zi(iprn2-1+ (ino2-1)* (nec+2)+2)
    ieq2 = zi(inueq2-1+ival2-1+1)
!
    if (ncmp2 .eq. 0) goto 1
!
    call antece(ino2, ma, tgeom, tailmi, epsi,&
                nbante, ino1)
!
!
    if (nbante .gt. 1) then
!
        call utmess('F', 'CALCULEL2_7')
!
    else
!
        if (nbante .eq. 0) then
!
!
            zr(iaval2-1+ieq2)=0.0d0
!
!
        else
!
            if (nbante .eq. 1) then
!
!
                ival1 = zi(iprn1-1+ (ino1-1)* (nec+2)+1)
                ncmp1 = zi(iprn1-1+ (ino1-1)* (nec+2)+2)
                ieq1 = zi(inueq1-1+ival1-1+1)
                val = zr(iaval1-1+ieq1)
!
                if (abs(val) .gt. tmin) then
                    zr(iaval2-1+ieq2)=val
                    nbnrcp = nbnrcp+1
                else
                    zr(iaval2-1+ieq2)=0.0d0
                endif
!
!
            endif
!
        endif
    endif
!
!
    1 end do
!
    if ((nbnrcp.lt.nbnaff) .and. (nbnrcp.gt.(nbnaff/2))) then
!
        call codent((nbnaff-nbnrcp), 'D0', diff(1:8))
        call codent((nbnaff), 'D0', chnaff(1:8))
!
        valk(1) = diff
        valk(2) = chnaff
        call utmess('A', 'CALCULEL2_8', nk=2, valk=valk)
!
    else
        if (nbnrcp .lt. (nbnaff/2)) then
!
            call utmess('A', 'CALCULEL2_9')
        endif
!
    endif
!
!
    if (nbnrcp .gt. nbnaff) then
!
        call utmess('F', 'CALCULEL2_10')
!
    endif
!
!
!
    call jedema()
end subroutine
