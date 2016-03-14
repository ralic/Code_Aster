subroutine op0051()
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
!     ------------------------------------------------------------------
!     OPERATEUR   POST_RELEVE_T
!     ------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
!
!     ------------------------------------------------------------------
!
! 0.3. ==> VARIABLES LOCALES
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rvcohe.h"
#include "asterfort/rvgacc.h"
#include "asterfort/rvgarg.h"
#include "asterfort/rvgchf.h"
#include "asterfort/rvmima.h"
#include "asterfort/rvmoye.h"
#include "asterfort/rvouex.h"
#include "asterfort/rvpar0.h"
#include "asterfort/rvpost.h"
#include "asterfort/rvvsup.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=6) :: nompro
    parameter ( nompro = 'OP0051' )
!
    integer :: ifm, niv
    integer :: ichef, iocc, iret, ivchf, jacc, jaccis, jaccr8, jchef, jtac
    integer :: jvac, n1, nbacce, nbchef, nbpost, nbresu, nbvchf
!
    real(kind=8) :: epsi
!
    character(len=2) :: codacc, dim
    character(len=6) :: mcf
    character(len=8) :: k8b, resuco, criter
    character(len=16) :: nomcmd, concep, ncheff, nchsym, option, k16
    character(len=19) :: latabl
    character(len=19) :: nch19
    character(len=24) :: xnumcp, xnomcp, vnomch, vcodop, xnovar
    character(len=24) :: naccis, naccr8, nch24, nlsmac, nlsnac
    aster_logical :: trouve
!     ------------------------------------------------------------------
!
!====
! 1. PREALABLES
!====
!
    call jemarq()
!
    ncheff = '&&'//nompro//'.CHAMP19'
    xnomcp = '&&'//nompro//'.NOM.COMPOSANTES'
    xnovar = '&&'//nompro//'.NOM.VARI       '
    xnumcp = '&&'//nompro//'.NUM.COMPOSANTES'
    vnomch = '&&'//nompro//'.NOM.CHAMPEFFECT'
    vcodop = '&&'//nompro//'.CODE.OPERATION '
    naccis = '&&'//nompro//'.ACCES.ENTIER   '
    naccr8 = '&&'//nompro//'.ACCES.REEL     '
    nlsmac = '&&'//nompro//'.MAILLES.ACTIVES'
    nlsnac = '&&'//nompro//'.NOEUDS .ACTIFS '
!
!====
!  2. RECUPERATION DES OPERANDES
!====
! 2.1. ==>  RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infmaj()
    call infniv(ifm, niv)
!
! 2.2. ==> LE CONCEPT DE SORTIE, SON TYPE, LA COMMANDE
!
    call getres(latabl, concep, nomcmd)
    if (niv .ge. 2) then
        call utmess('I', 'POSTRELE_1', sk=latabl)
    endif
!
! 2.3. ==> PHASE DE VERIFICATIONS SUPPLEMENTAIRES
!
    call rvvsup()
!
! 2.4. ==> PHASE DE VERIFICATION D'EXISTENCE DES ARGUMENTS
!
    call rvgarg(xnomcp, xnumcp, vnomch, vcodop, xnovar)
!
! 2.5. ==> PHASE D'INITIALISATION DE LA TABLE
!
    mcf = 'ACTION'
    call getfac(mcf, nbpost)
!
    call rvpar0(latabl(1:8), mcf, nbpost)
    dim = '  '
!
!====
! 3. TRAITEMENT EFFECTIF
!====
!============ DEBUT DE LA BOUCLE SUR LES POST-TRAITEMENTS ==============
!
    do iocc = 1, nbpost, 1
!
        call getvtx('ACTION', 'OPERATION', iocc=iocc, scal=k16, nbret=iret)
        if (k16 .eq. 'EXTREMA') then
            call rvmima(latabl, iocc)
            goto 3
        endif
        if (k16 .eq. 'MOYENNE_ARITH') then
            call rvmoye(latabl, iocc)
            goto 3
        endif

!       RESULTANTE n'a de sens que pour EXTRACTION et pour des forces nodales :
        if (k16 .ne. 'EXTRACTION') then
            call getvtx('ACTION', 'RESULTANTE', iocc=iocc, nbval=0, nbret=n1)
            if (n1.ne.0) call utmess('F','POSTRELE_67')
        endif
!
! 3.1. ==> VERIFICATION DE COHERENCE DES ARGUMENTS DE LA COMMANDE
!
        call rvcohe(xnumcp, xnomcp, vnomch, iocc, iret)
!
        if (iret .ne. 0) then
!
            call getvtx(mcf, 'MOYE_NOEUD', iocc=iocc, scal=k8b, nbret=n1)
!
!        --- EST-CE UN RESULTAT ? ---
!
            resuco = '        '
            call getvid(mcf, 'RESULTAT', iocc=iocc, scal=resuco, nbret=nbresu)
!
!
!
!        --- SAISIE DES CHAMPS EFFECTIFS A POST-TAITER ---
!
            if (nbresu .ne. 0) then
!
!           /* CAS D' UN RESULTAT */
!
                call getvtx(mcf, 'NOM_CHAM', iocc=iocc, scal=nchsym, nbret=n1)
                call getvtx(mcf, 'CRITERE', iocc=iocc, scal=criter, nbret=n1)
                call getvr8(mcf, 'PRECISION', iocc=iocc, scal=epsi, nbret=n1)
!
                call rvgacc(iocc, codacc, naccis, naccr8, nbacce)
!
                call jeveuo(naccis, 'L', jaccis)
                call jeveuo(naccr8, 'L', jaccr8)
!
                call rvgchf(epsi, criter, resuco, nchsym, codacc,&
                            zi(jaccis), zr(jaccr8), nbacce, ncheff)
!
!
                call jedetr(naccis)
                call jedetr(naccr8)
!
            else
!
!           /* CAS D' UN CHAMP DE GRANDEUR */
!
                call wkvect(ncheff//'.TYPACCE', 'V V K8', 1, jacc)
!
                zk8(jacc) = 'DIRECT  '
!
                call wkvect(ncheff//'.VALACCE', 'V V I', 1, jacc)
!
                zi(jacc) = 1
!
                call jecrec(ncheff//'.LSCHEFF', 'V V K24', 'NU', 'DISPERSE', 'VARIABLE',&
                            1)
!
                call jecroc(jexnum(ncheff//'.LSCHEFF', 1))
                call jeecra(jexnum(ncheff//'.LSCHEFF', 1), 'LONMAX', 1)
                call jeveuo(jexnum(ncheff//'.LSCHEFF', 1), 'E', jacc)
!
                call getvid(mcf, 'CHAM_GD', iocc=iocc, scal=zk24(jacc), nbret=n1)
!
                call dismoi('TYPE_CHAMP', zk24(jacc), 'CHAMP', repk=k8b)
                if (k8b(1:4) .eq. 'ELNO') then
                    call dismoi('NOM_OPTION', zk24(jacc), 'CHAMP', repk=option)
                endif
!
            endif
!
!        =====================================================
!        I     ON DISPOSE MAINTENANT DE :                    I
!        I                                                   I
!        I     LA XD NCHEFF DES CHAMPS EFFECTIFS MIS EN JEU  I
!        I     POUR L' OCCURENCE COURANTE (DE TYPE IDENTIQUE)I
!        I     DOCUMENTATION : CF RVGCHF                     I
!        I                                                   I
!        I     LES XD DE NOMS XNOMCP ET XNUMCP DES NOMS ET   I
!        I     NUMEROS DES CMPS MISES EN JEU                 I
!        I     DOCUMENTATION : CF RVGARG                     I
!        I                                                   I
!        I     DU VECTEUR VCODOP CONTENANT LES CODES DES     I
!        I     DES POST-TRAITEMENT PAR OCCURENCES            I
!        I     DOCUMENTATION : CF RVGARG                     I
!        I                                                   I
!        I  ON EST SUR QUE :                                 I
!        I                                                   I
!        I     TOUTES LES CMP MISES EN JEU SONT LEGALES      I
!        I                                                   I
!        I     LES CHAM_ELEM SONT BIEN "AUX NOEUDS"          I
!        I                                                   I
!        I     LES MAILLAGES, COURBES ET NOEUDS SONT         I
!        I     COHERANTS AVEC LES CHAMPS                     I
!        I                                                   I
!        I  ON DOIT SAISIR LE LIEU DU POST-TRAITEMENT :      I
!        I                                                   I
!        I     CE LIEU EST LE MEME POUR TOUS LES CHAMPS      I
!        I     EFFECTIFS                                     I
!        =====================================================
!
            call jelira(ncheff//'.LSCHEFF', 'NMAXOC', nbvchf)
!
            ivchf = 0
            trouve = .false.
!
300         continue
            if ((.not. trouve) .and. (ivchf .lt. nbvchf)) then
!
                ivchf = ivchf + 1
                ichef = 0
!
                call jelira(jexnum(ncheff//'.LSCHEFF', ivchf), 'LONMAX', nbchef)
                call jeveuo(jexnum(ncheff//'.LSCHEFF', ivchf), 'L', jchef)
!
310             continue
                if ((.not. trouve) .and. (ichef .lt. nbchef)) then
!
                    ichef = ichef + 1
!
                    nch24 = zk24(jchef + ichef-1)
!
                    if (nch24(1:1) .ne. '&') trouve = .true.
!
                    goto 310
!
                endif
!
                goto 300
!
            endif
!
            if (.not. trouve) then
                call utmess('F', 'POSTRELE_2', si=iocc)
            else
!
!           --- SAISIE DU LIEU DU POST-TRAITEMENT DE L' OCCURENCE ---
!
                call rvouex(mcf, iocc, nch24, xnomcp, nlsmac,&
                            nlsnac, iret)
!
                if (iret .eq. 0) then
                    call utmess('F', 'POSTRELE_3', si=iocc)
!
                else
!
                    call jeveuo(ncheff//'.TYPACCE', 'L', jtac)
                    call jeveuo(ncheff//'.VALACCE', 'L', jvac)
!
                    do ivchf = 1, nbvchf, 1
!
                        call jelira(jexnum(ncheff//'.LSCHEFF', ivchf), 'LONMAX', nbchef)
                        call jeveuo(jexnum(ncheff//'.LSCHEFF', ivchf), 'L', jchef)
!
!
                        do ichef = 1, nbchef
!
                            nch19 = zk24(jchef + ichef-1)(1:19)
!
                            call rvpost(mcf, iocc, dim, ivchf, ichef,&
                                        ncheff, xnomcp, resuco, nch19, nlsmac,&
                                        nlsnac, latabl, xnovar)
!
                        end do
!
                    end do
!
                endif
!
                call jeexin(nlsmac, n1)
                if (n1 .ne. 0) call jedetr(nlsmac)
!
                call jeexin(nlsnac, n1)
                if (n1 .ne. 0) call jedetr(nlsnac)
!
            endif
!
            call jedetr(ncheff//'.NOMRESU')
            call jedetr(ncheff//'.TYPACCE')
            call jedetr(ncheff//'.VALACCE')
            call jedetr(ncheff//'.LSCHEFF')
!
!
!
!
        endif
!
!
  3     continue
    end do
!
!============= FIN DE LA BOUCLE SUR LES POST-TRAITEMENTS ===============
!
    call jedema()
!
end subroutine
