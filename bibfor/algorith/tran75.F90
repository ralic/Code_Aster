subroutine tran75(nomres, typres, nomin, basemo)
    implicit none
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR DE RETOUR A LA BASE PHYSIQUE A PARTIR DE DONNEES
!     GENERALISEES DANS LE CAS D'UN CALCUL TRANSITOIRE
!     ------------------------------------------------------------------
! IN  : NOMRES : NOM UTILISATEUR POUR LA COMMANDE REST_BASE_PHYS
! IN  : TYPRES : TYPE DE RESULTAT : 'DYNA_TRANS'
! IN  : NOMIN  : NOM UTILISATEUR DU CONCEPT TRAN_GENE AMONT
! IN  : NOMCMD : NOM DE LA COMMANDE : 'REST_BASE_PHYS'
! IN  : BASEMO : NOM UTILISATEUR DU CONCEPT MODE_MECA AMONT
!                (SI CALCUL MODAL PAR SOUS-STRUCTURATION)
!                BLANC SINON
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/cnocre.h"
#include "asterfort/copmod.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/extrac.h"
#include "asterfort/fointe.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/idensd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdgep3.h"
#include "asterfort/mdgeph.h"
#include "asterfort/pteddl.h"
#include "asterfort/rbph01.h"
#include "asterfort/rbph02.h"
#include "asterfort/refdaj.h"
#include "asterfort/refdcp.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rstran.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcreb.h"
#include "asterfort/vtcrec.h"
#include "asterfort/vtdefs.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
! ----------------------------------------------------------------------
    integer :: i, j, itresu(8)
    integer :: foci, focf, fomi, fomf, fomo
    real(kind=8) :: r8b, epsi, alpha, xnorm, depl(6)
    character(len=1) :: typ1
    character(len=8) :: k8b, blanc, basemo, crit, interp, basem2, mailla, nomres
    character(len=8) :: nomin, nomcmp(6), mode, monmot(2), matgen, nomgd
    character(len=14) :: numddl
    character(len=16) :: typres, type(8), typcha, typbas(8), concep
    character(len=19) :: fonct, kinst, knume, prchno, prchn1, trange
    character(len=19) :: typref(8), prof
    character(len=24) :: matric, chamno, crefe(2), nomcha, chamn2, objve1
    character(len=24) :: objve2, objve3, objve4, chmod
    logical :: tousno, multap, leffor, prems
    integer :: iexi
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer ::  iarchi, ibid, ich, id
    integer ::  idec, idefm,  idresu,  ie
    integer :: ier, inocmp, inoecp, inuddl, inumno, ipsdel, ir
    integer :: irou,  jc,  jinst
    integer ::   jnume, jpsdel, jvec, linst, llcha
    integer :: lpsdel, lval2, lvale, n1, n2, n3
    integer :: n4, nbcham, nbd, nbdir, nbexci, nbinsg, nbinst
    integer :: nbmode, nbnoeu, ncmp, neq, nfonct, neq0
    complex(kind=8) :: cbid
    real(kind=8), pointer :: base(:) => null()
    integer, pointer :: ddl(:) => null()
    real(kind=8), pointer :: vectgene(:) => null()
    character(len=8), pointer :: fvit(:) => null()
    character(len=24), pointer :: refn(:) => null()
    character(len=8), pointer :: facc(:) => null()
    integer, pointer :: desc(:) => null()
    real(kind=8), pointer :: disc(:) => null()
    character(len=8), pointer :: fdep(:) => null()
!-----------------------------------------------------------------------
    data blanc    /'        '/
    data chamn2   /'&&TRAN75.CHAMN2'/
    data nomcmp   /'DX      ','DY      ','DZ      ',&
     &               'DRX     ','DRY     ','DRZ     '/
!     ------------------------------------------------------------------
    cbid = dcmplx(0.d0, 0.d0)
    call jemarq()
    mode = basemo
    trange = nomin
    call gettco(nomin, concep)
!
    nomcha=' '
    numddl=' '
    prchno=' '
!
!     --- RECHERCHE SI UNE ACCELERATION D'ENTRAINEMENT EXISTE ---
    fonct = ' '
    nfonct = 0
    call getvid(' ', 'ACCE_MONO_APPUI', scal=fonct, nbret=nfonct)
    if (nfonct .ne. 0) then
!
        call getvr8(' ', 'DIRECTION', nbval=0, nbret=nbd)
        nbdir = -nbd
        call getvr8(' ', 'DIRECTION', nbval=nbdir, vect=depl, nbret=nbd)
        xnorm = 0.d0
        do id = 1, nbdir
            xnorm = xnorm + depl(id) * depl(id)
        end do
        xnorm = sqrt(xnorm)
        if (xnorm .lt. r8prem()) then
            call utmess('F', 'ALGORITH9_81')
        endif
        do id = 1, nbdir
            depl(id) = depl(id) / xnorm
        end do
!
    endif
!
!     --- RECUPERATION DES ENTITES DU MAILLAGE SUR LESQUELLES ---
!     ---                PORTE LA RESTITUTION                 ---
    tousno = .true.
    call getvtx(' ', 'GROUP_NO', nbval=0, nbret=n1)
    call getvtx(' ', 'NOEUD', nbval=0, nbret=n2)
    call getvtx(' ', 'GROUP_MA', nbval=0, nbret=n3)
    call getvtx(' ', 'MAILLE', nbval=0, nbret=n4)
    if (n1+n2+n3+n4 .ne. 0) tousno = .false.
!
!     --- RECUPERATION DE LA BASE MODALE ---
!
!
    call jeveuo(trange//'.DESC', 'L', vi=desc)
    nbmode = desc(2)
!
!
    if (mode .eq. blanc) then
!
        call dismoi('BASE_MODALE', trange, 'RESU_DYNA', repk=basemo, arret='C',&
                    ier=ir)
        call dismoi('REF_RIGI_PREM', trange, 'RESU_DYNA', repk=matgen, arret='C',&
                    ier=ir)
!
        if (matgen(1:8) .ne. blanc) then
            call dismoi('NUME_DDL', basemo, 'RESU_DYNA', repk=numddl, arret='C',&
                        ier=ir)
            call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=matric, arret='C',&
                        ier=ir)
!
            if (numddl .eq. blanc) then
                if (matric .ne. blanc) then
                    call dismoi('NOM_NUME_DDL', matric, 'MATR_ASSE', repk=numddl)
                endif
            endif
            prchno=numddl//'.NUME'
            call dismoi('NOM_GD', numddl, 'NUME_DDL', repk=nomgd)
            call dismoi('NOM_MAILLA', numddl, 'NUME_DDL', repk=mailla)
            if (tousno) call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neq)
        else
!          -- POUR LES CALCULS SANS MATRICE GENERALISEE
!             (PROJ_MESU_MODAL)
            call dismoi('NUME_DDL', basemo, 'RESU_DYNA', repk=matric, arret='C',&
                        ier=ir)
            if (matric(1:8) .eq. blanc) then
                call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=matric)
                call dismoi('NOM_NUME_DDL', matric, 'MATR_ASSE', repk=numddl)
            else
                numddl = matric(1:8)
            endif
            prchno=numddl//'.NUME'
            call jeveuo(numddl//'.NUME.REFN', 'L', vk24=refn)
            matric = refn(1)
            mailla = matric(1:8)
            call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=matric)
            if (tousno) call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neq)
        endif
!
        basem2 = basemo
!
!
    else
!         --- BASE MODALE CALCULEE PAR SOUS-STRUCTURATION
!
        call rsexch('F', basemo, 'DEPL', 1, chmod,&
                    ir)
        chmod = chmod(1:19)//'.REFE'
        call dismoi('NOM_GD', chmod, 'CHAM_NO', repk=nomgd)
        call dismoi('PROF_CHNO', chmod, 'CHAM_NO', repk=prchno)
        call jeveuo(chmod, 'L', llcha)
        mailla = zk24(llcha)(1:8)
        crefe(1) = zk24(llcha)
        crefe(2) = zk24(llcha+1)
        if (tousno) call jelira(crefe(2)(1:19)//'.NUEQ', 'LONMAX', neq)
        basem2 = ' '
    endif
!
! --- MULTI-APPUIS
!
    multap = .false.
    monmot(1)=blanc
    monmot(2)=blanc
    call getvtx(' ', 'MULT_APPUI', scal=monmot(1), nbret=n1)
    call getvtx(' ', 'CORR_STAT', scal=monmot(2), nbret=n2)
!
    if (monmot(1) .eq. 'OUI' .or. monmot(2) .eq. 'OUI') multap = .true.
!
!     ---   RECUPERATION DES VECTEURS DEPLACEMENT, VITESSE ET   ---
!     --- ACCELERATION GENERALISES SUIVANT LES CHAMPS SOUHAITES ---
    call rbph01(trange, nbcham, type, itresu, nfonct,&
                basem2, typref, typbas, tousno, multap)
!
!     --- RECUPERATION DES NUMEROS DES NOEUDS ET DES DDLS ASSOCIES ---
!     ---         DANS LE CAS D'UNE RESTITUTION PARTIELLE          ---
!
    if (.not. tousno) then
        objve1 = '&&TRAN75.NUME_NOEUD  '
        objve2 = '&&TRAN75.NOM_CMP     '
        objve3 = '&&TRAN75.NB_NEQ      '
        objve4 = '&&TRAN75.NUME_DDL    '
        call rbph02(mailla, numddl, chmod, nomgd, neq,&
                    nbnoeu, objve1, ncmp, objve2, objve3,&
                    objve4)
        call jeveuo(objve1, 'L', inumno)
        call jeveuo(objve2, 'L', inocmp)
        call jeveuo(objve3, 'L', inoecp)
        call jeveuo(objve4, 'L', inuddl)
    endif
!
! --- MULTI-APPUIS : RECUP DE L EXCITATION ET DE PSI*DELTA
!
    if (multap) then
        call jeveuo(trange//'.FDEP', 'L', vk8=fdep)
        call jeveuo(trange//'.FVIT', 'L', vk8=fvit)
        call jeveuo(trange//'.FACC', 'L', vk8=facc)
        call jeveuo(trange//'.IPSD', 'L', ipsdel)
        call jelira(trange//'.FDEP', 'LONMAX', nbexci)
        nbexci = nbexci/2
        if (tousno) then
            call vtcreb(chamn2, numddl, 'V', 'R', neq)
            chamn2(20:24) = '.VALE'
            call jeveuo(chamn2, 'E', lval2)
            lpsdel = ipsdel
        else
            call wkvect('&&TRAN75.PSI_DELTA', 'V V R', neq, jpsdel)
            idec = 0
            do i = 1, nbnoeu
                do j = 1, ncmp
                    if (zi(inoecp-1+(i-1)*ncmp+j) .eq. 1) then
                        idec = idec + 1
                        zr(jpsdel+idec-1) = zr(ipsdel+zi(inuddl+idec- 1)-1)
                    endif
                end do
            end do
            call wkvect('&&TRAN75.VAL2', 'V V R', neq, lval2)
            lpsdel = jpsdel
        endif
    endif
!
!     --- RECUPERATION DES INSTANTS ---
!
    call getvtx(' ', 'CRITERE', scal=crit, nbret=n1)
    call getvr8(' ', 'PRECISION', scal=epsi, nbret=n1)
    call getvtx(' ', 'INTERPOL', scal=interp, nbret=n1)
!
    knume = '&&TRAN75.NUM_RANG'
    kinst = '&&TRAN75.INSTANT'
    call rstran(interp, trange, ' ', 1, kinst,&
                knume, nbinst, irou)
    if (irou .ne. 0) then
        call utmess('F', 'UTILITAI4_24')
    endif
    call jeexin(kinst, ir)
    if (ir .gt. 0) then
        call jeveuo(kinst, 'L', jinst)
        call jeveuo(knume, 'L', jnume)
    endif
!
!     --- CREATION DE LA SD RESULTAT ---
    call rscrsd('G', nomres, typres, nbinst)
!
!     --- RESTITUTION SUR LA BASE REELLE ---
!
! VERIFICATION QU'IL Y UN DE CES MOTS CLEFS :
!  'LIST_INST', 'LIST_FREQ', 'INST' ou 'FREQ'
! A MOINS QUE L'ON NE SOIT DANS UN CAS DE DOUBLE RESTITUTION
! APRES UNE DOUBLE PROJECTION (PRESENCE DU MOT CLEF 'MODE_MECA')
    foci = 0
    focf = 0
    fomi = 0
    fomf = 0
    fomo = 0
    call getvid(' ', 'LIST_INST', scal=k8b, nbret=foci)
    call getvid(' ', 'LIST_FREQ', scal=k8b, nbret=focf)
    call getvr8(' ', 'INST', scal=r8b, nbret=fomi)
    call getvr8(' ', 'FREQ', scal=r8b, nbret=fomf)
    call getvid(' ', 'MODE_MECA', scal=k8b, nbret=fomo)
    if ((interp(1:3).ne.'NON') .and.&
        (&
        foci .eq. 0 .and. focf .eq. 0 .and. fomi .eq. 0 .and. fomf .eq. 0 .and. fomo .eq. 0&
        )) then
        call utmess('F', 'ALGORITH10_95')
    endif
    call jeveuo(trange//'.DISC', 'L', vr=disc)
    call jelira(trange//'.DISC', 'LONMAX', nbinsg)
    AS_ALLOCATE(vr=vectgene, size=nbmode)
    neq0 = neq
    do ich = 1, nbcham
        leffor=.true.
        if (type(ich) .eq. 'DEPL' .or. type(ich) .eq. 'VITE' .or. type(ich) .eq. 'ACCE' .or.&
            type(ich) .eq. 'ACCE_ABSOLU') leffor=.false.
!
!            --- RECUPERATION DES DEFORMEES MODALES ---
!
        typcha = typbas(ich)
        call rsexch('F', basemo, typcha, 1, nomcha,&
                    ir)
        nomcha = nomcha(1:19)//'.VALE'
        call jeexin(nomcha, ibid)
        if (ibid .gt. 0) then
            nomcha(20:24)='.VALE'
        else
            nomcha(20:24)='.CELV'
        endif
!
        if (leffor) then
            call jelira(nomcha, 'LONMAX', neq)
        else
            neq = neq0
        endif
!
        AS_ALLOCATE(vr=base, size=nbmode*neq)
        if (tousno) then
            call copmod(basemo, champ=typcha, numer=prchno(1:14), bmodr=base, &
                                nequa=neq, nbmodes=nbmode)
        else
            do j = 1, nbmode
                call rsexch('F', basemo, typcha, j, nomcha,&
                            ir)
                call jeexin(nomcha(1:19)//'.VALE', iexi)
!              TOUSNO=.FALSE. => ON NE S'INTERESSE QU'AUX CHAM_NO :
                ASSERT(iexi.gt.0)
                call jelira(nomcha(1:19)//'.VALE', 'TYPE', cval=typ1)
                ASSERT(typ1.eq.'R')
!
!              SI NOMCHA N'A PAS LA BONNE NUMEROTATION, ON ARRETE TOUT :
                ASSERT(prchno.ne.' ')
                call dismoi('PROF_CHNO', nomcha, 'CHAM_NO', repk=prchn1)
                ASSERT(idensd('PROF_CHNO', prchno, prchn1))
!
                nomcha(20:24)='.VALE'
                call jeveuo(nomcha, 'L', idefm)
                idec = 0
                do i = 1, nbnoeu
                    do jc = 1, ncmp
                        if (zi(inoecp-1+(i-1)*ncmp+jc) .eq. 1) then
                            idec = idec + 1
                            base(1+(j-1)*neq+idec-1) = zr( idefm+zi( inuddl+idec-1)-1 )
                        endif
                    end do
                end do
            end do
        endif
        iarchi = 0
        if (interp(1:3) .eq. 'NON') then
            call jeexin(trange//'.ORDR', ir)
            if (ir .ne. 0 .and. zi(jnume) .eq. 1) iarchi = -1
        endif
        idresu = itresu(ich)
        prems=.true.
        do i = 0, nbinst-1
            iarchi = iarchi + 1
            call rsexch(' ', nomres, type(ich), iarchi, chamno,&
                        ir)
            if (ir .eq. 0) then
                call utmess('A', 'ALGORITH2_64', sk=chamno)
            else if (ir .eq. 100) then
                if (tousno) then
                    if (mode .eq. blanc) then
                        if (leffor) then
                            call vtdefs(chamno, typref(ich), 'G', 'R')
                        else
                            call vtcreb(chamno, numddl, 'G', 'R', neq)
                        endif
                    else
                        call vtcrec(chamno, chmod, 'G', 'R', neq)
                    endif
                else
                    if (prems) then
                        prems=.false.
                        call cnocre(mailla, nomgd, nbnoeu, zi(inumno), ncmp,&
                                    zk8(inocmp), zi(inoecp), 'G', ' ', chamno)
                        call dismoi('PROF_CHNO', chamno, 'CHAM_NO', repk=prof)
                    else
                        call cnocre(mailla, nomgd, nbnoeu, zi( inumno), ncmp,&
                                    zk8(inocmp), zi(inoecp), 'G', prof, chamno)
                    endif
                endif
            else
                ASSERT(.false.)
            endif

            chamno(20:24) = '.VALE'
            call jeexin(chamno, ibid)
            if (ibid .le. 0) chamno(20:24) = '.CELV'

            call jeveuo(chamno, 'E', lvale)
!
            if (leffor .or. .not.tousno) call jelira(chamno, 'LONMAX', neq)
            if (interp(1:3) .ne. 'NON') then
                call extrac(interp, epsi, crit, nbinsg, disc,&
                            zr(jinst+i), zr(idresu), nbmode, vectgene, ibid)
                call mdgeph(neq, nbmode, base, vectgene, zr(lvale))
            else
                call mdgeph(neq, nbmode, base, zr(idresu+(zi( jnume+i)-1)*nbmode),&
                            zr(lvale))
            endif
            if (multap) then
                if (type(ich) .eq. 'DEPL') call mdgep3(neq, nbexci, zr( lpsdel), zr(jinst+i),&
                                                       fdep, zr(lval2))
                if (type(ich) .eq. 'VITE') call mdgep3(neq, nbexci, zr( lpsdel), zr(jinst+i),&
                                                       fvit, zr(lval2))
                if (type(ich) .eq. 'ACCE') call mdgep3(neq, nbexci, zr( lpsdel), zr(jinst+i),&
                                                       facc, zr(lval2))
                if (type(ich) .eq. 'ACCE_ABSOLU') call mdgep3(neq, nbexci, zr(lpsdel),&
                                                              zr(jinst+i), facc,&
                                                              zr(lval2))
                do ie = 1, neq
                    zr(lvale+ie-1)=zr(lvale+ie-1)+zr(lval2+ie-1)
                end do
            endif
!            --- PRISE EN COMPTE D'UNE ACCELERATION D'ENTRAINEMENT
            if (type(ich) .eq. 'ACCE_ABSOLU' .and. nfonct .ne. 0) then
                ir = 0
                call fointe('F', fonct, 1, 'INST', zr(jinst+i),&
                            alpha, ier)
!               --- ACCELERATION ABSOLUE = RELATIVE + ENTRAINEMENT
                call wkvect('&&TRAN75.VECTEUR', 'V V R', neq, jvec)
                AS_ALLOCATE(vi=ddl, size=neq*nbdir)
                call pteddl('NUME_DDL', numddl, nbdir, nomcmp, neq,&
ddl)
                do id = 1, nbdir
                    do ie = 0, neq-1
                        zr(jvec+ie) = zr(jvec+ie) + ddl(1+neq*(id-1) +ie)*alpha*depl(id)
                    end do
                end do
                do ie = 0, neq-1
                    zr(lvale+ie) = zr(lvale+ie) + zr(jvec+ie)
                end do
                call jedetr('&&TRAN75.VECTEUR')
                AS_DEALLOCATE(vi=ddl)
            endif
            call rsnoch(nomres, type(ich), iarchi)
            call rsadpa(nomres, 'E', 1, 'INST', iarchi,&
                        0, sjv=linst, styp=k8b)
            zr(linst) = zr(jinst+i)
        end do
        AS_DEALLOCATE(vr=base)
    end do
!
!
    if (mode .eq. blanc) then
        call refdcp(basemo, nomres)
    else
        call refdaj(' ', nomres, -1, ' ', 'INIT',&
                    ' ', ir)
    endif
!
! --- MENAGE
    call detrsd('CHAM_NO', '&&TRAN75.CHAMN2')
    call jedetr('&&TRAN75.NUME_NOEUD  ')
    call jedetr('&&TRAN75.NOM_CMP     ')
    call jedetr('&&TRAN75.NB_NEQ      ')
    call jedetr('&&TRAN75.NUME_DDL    ')
    call jedetr('&&TRAN75.PSI_DELTA')
    call jedetr('&&TRAN75.VAL2')
    call jedetr('&&TRAN75.NUM_RANG')
    call jedetr('&&TRAN75.INSTANT')
    AS_DEALLOCATE(vr=vectgene)
!
    call titre()
!
    call jedema()
end subroutine
