subroutine op0093()
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!
!     OPERATEUR MODE_STATIQUE
!
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/arch93.h"
#include "asterfort/cresol.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/modsta.h"
#include "asterfort/moin93.h"
#include "asterfort/mstget.h"
#include "asterfort/mtcopy.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/preres.h"
#include "asterfort/psmo93.h"
#include "asterfort/utmess.h"
#include "asterfort/vpcrea.h"
#include "asterfort/wkvect.h"
    integer :: ibid, neq, lmatr, ifm, niv, iret, nra, nma, nbpsmo, nbmodd
    integer :: nbmost, lddld, i, lmodd, nbmodf, nbfona, lddlf, lmodf, nbmoad
    integer :: nbmoda, nbmoin, nbmodi, massfa
    character(len=8) :: resu, nomma
    character(len=14) :: nume
    character(len=16) :: k16bid, concep
    character(len=19) :: raide, masse, amor, numedd, matpre, solveu, raidfa
    character(len=24) :: valk, mocb, moatta, moaimp, moauni, mointf, ddlcb
    character(len=24) :: ddlmn, vefreq, ddlac
!     ------------------------------------------------------------------
    call jemarq()
!
!------------------------------C
!--                          --C
!-- INITIALISATIONS DIVERSES --C
!--                          --C
!------------------------------C
!
    masse = ' '
    amor = ' '
    raide = ' '
    massfa=0
    nbmodd=0
    nbmodf=0
    nbmoda=0
    nbmoad=0
    nbmodi=0
!
!---------------------------------------------C
!--                                         --C
!-- RECUPERATION DES INFOS ET FACTORISATION --C
!--                                         --C
!---------------------------------------------C
!
    call getres(resu, concep, K16bid)
!
    call getvid(' ', 'MATR_RIGI', scal=raide, nbret=nra)
    call getvid(' ', 'MATR_MASS', scal=masse, nbret=nma)
!
    call getfac('PSEUDO_MODE', nbpsmo)
    if (nbpsmo .ne. 0) then
        if (nma .eq. 0) then
            call utmess('F', 'ALGELINE2_77')
        endif
    endif
!
!     -- CREATION DU SOLVEUR :
    solveu='&&OP0093.SOLVEUR'
    call cresol(solveu)
!
!
!     --- COMPATIBILITE DES MODES (DONNEES ALTEREES) ---
    call exisd('MATR_ASSE', raide, ibid)
    if (ibid .ne. 0) then
        call dismoi('NOM_NUME_DDL', raide, 'MATR_ASSE', repk=numedd)
    else
        numedd=' '
    endif
    call vpcrea(0, resu, masse, amor, raide,&
                numedd, ibid)
!
    call infmaj()
    call infniv(ifm, niv)
!
    call dismoi('NOM_MAILLA', raide, 'MATR_ASSE', repk=nomma)
    call dismoi('NOM_NUME_DDL', raide, 'MATR_ASSE', repk=nume)
    call dismoi('NB_EQUA', raide, 'MATR_ASSE', repi=neq)
!
!-- FACTORISATION DE LA MATRICE DE RAIDEUR
    raidfa = '&&MOIN93.RAIDFA'
    matpre = '&&MOIN93.MATPRE'
    call mtdefs(raidfa, raide, 'V', ' ')
    call mtcopy(raide, raidfa, iret)
    call mtdscr(raidfa)
    call jeveuo(raidfa(1:19)//'.&INT', 'E', lmatr)
    call preres(solveu, 'V', iret, matpre, raidfa,&
                ibid, -9999)
    if (iret .eq. 2) then
        valk = raide
        call utmess('F', 'ALGELINE4_37', sk=valk)
    endif
!
    call getfac('MODE_STAT', nbmost)
    call getfac('FORCE_NODALE', nbfona)
    call getfac('PSEUDO_MODE', nbpsmo)
    call getfac('MODE_INTERF', nbmoin)
!
!-------------------------------------C
!--                                 --C
!-- CALCUL DES DIFERENTES DEFORMEES --C
!--                                 --C
!-------------------------------------C
    ddlcb='&&OP0093.DDL_STAT_DEPL'
    mocb='&&OP0093.MODE_STAT_DEPL'
    ddlmn='&&OP0093.DDL_STAT_FORC'
    moatta='&&OP0093.MODE_STAT_FORC'
    moauni='&&OP0093.MODE_STAT_ACCU'
    moaimp='&&OP0093.MODE_ACCE_IMPO'
    ddlac=   '&&OP0093.DDL_ACCE_IMPO'
    mointf='&&MOIN93.MODE_INTF_DEPL'
    vefreq='&&MOIN93.FREQ_INTF_DEPL'
!
!-- CALCUL DES MODES DE CONTRAINTES (METHODE CRAIG & BAMPTON)
    if (nbmost .gt. 0) then
        call wkvect(ddlcb, 'V V I', neq, lddld)
        call mstget(raide, 'MODE_STAT', nbmost, zi(lddld))
        do i = 0, neq-1
            nbmodd = nbmodd + zi(lddld+i)
        end do
        call wkvect(mocb, 'V V R', neq*nbmodd, lmodd)
        call modsta('DEPL', raidfa, matpre, solveu, ibid,&
                    nume, zi(lddld), [0.d0], neq, nbmodd,&
                    zr(lmodd))
    endif
!
!-- CALCUL DES MODES D'ATTACHE (METHODE MAC NEAL)
    if (nbfona .gt. 0) then
        call wkvect(ddlmn, 'V V I', neq, lddlf)
        call mstget(raide, 'FORCE_NODALE', nbfona, zi(lddlf))
        do i = 0, neq-1
            nbmodf = nbmodf + zi(lddlf+i)
        end do
        call wkvect(moatta, 'V V R', neq*nbmodf, lmodf)
        call modsta('FORC', raidfa, matpre, solveu, ibid,&
                    nume, zi(lddlf), [0.d0], neq, nbmodf,&
                    zr(lmodf))
    endif
!
!-- CALCUL DES PSEUDO MODES
    if (nbpsmo .gt. 0) then
        call mtdscr(masse)
        massfa=2
        call psmo93(solveu, masse, raide, raidfa, nume,&
                    nbpsmo, nbmoda, nbmoad)
    endif
!
!-- CALCUL DES MODES D'INTERFACE
    if (nbmoin .gt. 0) then
        if (massfa .lt. 1) then
            call mtdscr(masse)
        endif
        call getvis('MODE_INTERF', 'NBMOD', iocc=1, scal=nbmodi, nbret=ibid)
        call moin93(masse, raide, raidfa, nbmodi, mointf,&
                    vefreq)
    endif
!
!-----------------------------C
!--                         --C
!-- ARCHIVAGE DES RESULTATS --C
!--                         --C
!-----------------------------C
!
    call arch93(resu, concep, nume, raide, nbmodd,&
                nbmodf, nbmoda, nbmoad, nbmodi, nbpsmo)
!
!
    call jedetr(ddlcb)
    call jedetr(mocb)
    call jedetr(ddlmn)
    call jedetr(moatta)
    call jedetr(moauni)
    call jedetr(moaimp)
    call jedetr(ddlac)
    call jedetr(mointf)
    call jedetr(vefreq)
!
    call jedema()
!
end subroutine
