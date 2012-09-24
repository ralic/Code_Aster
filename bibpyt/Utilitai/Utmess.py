#@ MODIF Utmess Utilitai  DATE 24/09/2012   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
# (AT YOUR OPTION) ANY LATER VERSION.
#
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.
#
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
# ======================================================================
# RESPONSABLE COURTOIS M.COURTOIS

import os
import sys
import traceback
import re

# protection pour eficas
try:
    import aster
    import aster_core
    from aster import error
    aster_exists = True
except:
    aster_exists = False
    class error(Exception):
        pass


from Messages.context_info import message_context_concept
from Utilitai.string_utils import cut_long_lines, copy_text_to, clean_string
from Execution.strfunc import convert, ufmt, to_unicode
from Execution.E_Exception import ST

from Noyau.N_types import force_list
from Noyau.N_utils import Singleton

CENTER = 1
DECORATED = 2
ALL_UNIT = 4

MAXLENGTH = 132
LINE_WITH = 80

contacter_assistance = _(u"""
Il y a probablement une erreur dans la programmation.
Veuillez contacter votre assistance technique.""")

# voir en fin de fin les faux appels à UTMESS pour la vérification des messages


def list_unit(code):
    """Retourne la liste des noms de fichiers (logiques) sur lesquels doit
    etre imprimé le message.
    """
    # 'D' pour afficher un diagnostic 'F' sans les effets
    # 'Z' levée d'exception
    d = {
        'E' : ('ERREUR', 'MESSAGE', 'RESULTAT'),
        'I' : ('MESSAGE',),
        'A' : ('MESSAGE', 'RESULTAT'),
    }
    d['F'] = d['S'] = d['Z'] = d['D'] = d['E']
    d['X'] = d['A']
    return d.get(code, d['F'])


class MESSAGE_LOGGER(Singleton):
    """Classe gérant l'impression de messages.
    On ne crée qu'une instance de ce type (singleton).
    Cette instance est accessible dans le module aster_core pour les appels
    depuis le fortran.
    """
    def __init__(self):
        """Initialisation
        """
        self.init_buffer()

        # est-ce qu'une erreur <E> s'est produite
        self.erreur_E = False

        # compteur des alarmes émises { 'id_alarm' : count }
        self.nmax_alarm = 5
        self.count_alarm = {}         # dans la commande courante (pour arret à 5)
        self.count_alarm_tot = {}     # au total

        # alarmes à ignorer, à masquer (on ne les compte pas temporairement)
        self._ignored_alarm = {}
        self._hidden_alarm  = {}

        # on prépare le dictionnaire des valeurs par défaut des arguments (dicarg) :
        self.default_args = {}
        # initialisation des 50 premiers
        for i in range(1, 51):
            self.default_args['i%d' % i] = 99999999
            self.default_args['r%d' % i] = 9.9999E99
            self.default_args['k%d' % i] = u'xxxxxx'
        # mettre en cache les messages 'I' (et uniquement 'I')
        self._cache_txt = {}
        # arguments mpi : ligne de commande à envoyer au proc #0
        self._mpi_rank = None
        self.init_mpi_error()

    def init_mpi_error(self):
        """Stocke les informations nécessaires pour la gestion des erreurs en MPI."""
        if not aster_exists:
            return
        rank = aster_core.mpi_info()[0]
        self._mpi_rank = aster_core._USE_MPI and rank or None
        import platform
        node = platform.node()

    def __call__(self, *args, **kwargs):
        """Raccourci pour simplifier l'appel depuis astermodule.c et UTMESS.
        """
        self.print_message(*args, **kwargs)


    def print_message(self, code, idmess, valk=(), vali=(), valr=(), exc_num=None,
                            exception=False, print_as=None, cc=None):
        """Appelé par la routine fortran U2MESG ou à la fonction python UTMESS
        pour afficher un message.
        L'impression de ce message est différée si le `code` est suivi d'un "+".
            code  : 'A', 'E', 'S', 'F', 'I'
            idmess : identificateur du message
            valk, vali, valr : liste des chaines, entiers ou réels.
        Si exception==True, on lève une exception en cas d'erreur, sinon
        c'est l'appelant qui devra s'en charger (dans le C a priori).
        'print_as' : cf. print_buffer_content.
        """
        # le '+' n'a pas de sens pour les messages 'I'.
        if code == "I+":
            code = "I"
        if code == 'I':
            msg = self._cache_txt.get(idmess)
            if msg:
                try:
                    self.affiche('MESSAGE', msg % self.build_dict_args(valk, vali, valr))
                    return
                except:
                    # le formattage 'brut' échoue, on passera par une conversion complète
                    pass
        # récupération du texte du message
        dictmess = self.get_message(code, idmess, valk, vali, valr, exc_num)

        # on le met dans le buffer
        self.add_to_buffer(dictmess)

        # si on n'attend pas une suite, ...
        if len(code) < 2 or code[1] != '+':
            # mise à jour des compteurs
            self.update_counter()

            # on imprime le message en attente
            self.print_buffer_content(print_as, cc)

            if exception and code[0] in ('S', 'F'):
                if self._mpi_rank is not None:
                    aster_core.mpi_warn()
                if self._mpi_rank == 0:
                    l_unit = list_unit('F')
                    txt = _(u"On ne peut pas lever d'exception dans une exécution MPI.")
                    for unite in l_unit:
                        self.affiche(unite, txt)
                exc_typ = dictmess.get('exc_typ')
                if exc_typ:
                    raise exc_typ(idmess, valk, vali, valr)
                raise error(idmess, valk, vali, valr)

        return None


    def build_dict_args(self, valk, vali, valr):
        """Construit le dictionnaire de formatage du message.
        """
        # homogénéisation : uniquement des tuples + strip des chaines de caractères
        valk, vali, valr = map(force_list, (valk, vali, valr))
        valk    = [k.strip() for k in valk]

        # variables passées au message
        dicarg = self.default_args.copy()
        for i in range(1, len(valk)+1):
            dicarg['k%d' % i] = to_unicode(valk[i-1])
        for i in range(1, len(vali)+1):
            dicarg['i%d' % i] = vali[i-1]
        for i in range(1, len(valr)+1):
            dicarg['r%d' % i] = valr[i-1]
        # valeur spéciale : ktout = concaténation de toutes les chaines
        dicarg['ktout'] = ' '.join(valk)

        return dicarg


    def get_message(self, code, idmess, valk=(), vali=(), valr=(), exc_num=None):
        """Retourne le texte du message dans un dictionnaire dont les clés sont :
            'code', 'id_message', 'corps_message'
        """
        # décodage : idmess => (catamess, numess)
        idmess  = idmess.strip()
        x = idmess.split("_")
        assert len(x) > 1, idmess
        catamess = '_'.join(x[0:-1]).lower()
        numess = int(x[-1])
        assert numess > 0 and numess < 100, idmess

        # import catamess => cata_msg
        try:
            mod = __import__('Messages.%s' % catamess, globals(), locals(), [catamess])
            # si le dictionnaire n'existe pas, on alertera au moment du formatage.
            cata_msg = getattr(mod, 'cata_msg', {})
        except Exception, msg:
            # doit permettre d'éviter la récursivité (catamess réservé à Utmess)
            if catamess != 'catamess':
                self.print_message('A', 'CATAMESS_57', valk=(catamess, str(msg)))
            cata_msg = {}

        # corps du message
        fmt_msg = '?'
        try:
            dicarg = self.build_dict_args(valk, vali, valr)

            # cata_msg[num] = 'format'
            #              ou { 'message' : 'format',
            #                   'flags' : 'DECORATED | CENTER',
            #                   'context' : 'éléments de contexte' }
            if type(cata_msg[numess]) == dict:
                fmt_msg  = cata_msg[numess]['message']
                flags = eval( cata_msg[numess].get('flags', 0) )
                ctxt_msg = cata_msg[numess].get('context', None)
            else:
                fmt_msg  = cata_msg[numess]
                flags = 0
                ctxt_msg = None

            dictmess = {
                'code'          : code,
                'flags'         : flags,
                'id_message'    : idmess,
                'corps_message' : ufmt(fmt_msg, dicarg),
                'context_info'  : self.get_context(ctxt_msg, idmess, dicarg),
            }
            if code == 'I':
                self._cache_txt[idmess] = convert(fmt_msg)
        except Exception, msg:
            if code == 'I':
                code = 'A'
            dictmess = {
                'code'          : code,
                'flags'         : 0,
                'id_message'    : idmess,
                'corps_message' : _(u"""Erreur de programmation.
Le message %s n'a pas pu être formaté correctement.
Arguments :
    entiers : %s
    réels   : %s
    chaines : %s

    format  : %s
--------------------------------------------------------------------------
%s
Exception : %s
--------------------------------------------------------------------------

%s"""),
                'context_info'  : '',
            }
            args = (idmess, vali, valr, valk, fmt_msg,
                    ''.join(traceback.format_tb(sys.exc_traceback)),
                    msg, contacter_assistance)
            # cette étape ne doit jamais faire planter !
            try:
                dictmess['corps_message'] = dictmess['corps_message'] % args
            except Exception, exc:
                dictmess['corps_message'] = repr(args)
        # limite la longueur des lignes
        dictmess['corps_message'] = cut_long_lines(dictmess['corps_message'], MAXLENGTH)
        # type d'exception
        if exc_num:
            dictmess['exc_name'], dictmess['exc_typ'] = ST.get_exception_name(exc_num)
        return dictmess


    def GetText(self, *args, **kwargs):
        """Retourne le texte du message pret a etre imprime.
        """
        return self.format_message(self.get_message(*args, **kwargs))

    def init_buffer(self):
        """Initialise le buffer.
        """
        self._buffer = []

    def add_to_buffer(self, dictmess):
        """Ajoute le message décrit dans le buffer en vue d'une impression
        ultérieure.
        """
        self._buffer.append(dictmess)

    def get_current_code(self):
        """Retourne le code du message du buffer = code du message le plus grave
        (cf. dgrav)
        """
        dgrav = { '?' : -9, 'I' : 0, 'A' : 1, 'S' : 4, 'Z' : 4, 'E' : 6,
                  'D' :  9, 'F' : 10 }

        current = '?'
        exc_name = None
        exc_typ = None
        for dictmess in self._buffer:
            code = dictmess['code'][0]
            if dgrav.get(code, -9) > dgrav.get(current, -9):
                current = code
            exc_name = exc_name or dictmess.get('exc_name')
            exc_typ = exc_typ or dictmess.get('exc_typ')
        return current, exc_name, exc_typ

    def get_current_flags(self):
        """Retourne les flags du message du buffer = flags du premier."""
        return self._buffer[0]['flags']

    def get_current_id(self):
        """Retourne l'id du message du buffer = id du premier message
        """
        return self._buffer[0]['id_message']

    def print_buffer_content(self, print_as=None, cc=None):
        """Extrait l'ensemble des messages du buffer dans un dictionnaire unique,
        imprime le message, et vide le buffer pour le message suivant.
            - code : celui du message le plus grave (cf. dgrav)
            - id   : celui du premier message qui est affiché
            - corps : concaténation de tous les messages.
        'print'_as permet d'imprimer un message sur des fichiers autres que les fichiers
        habituels de 'code'. Par ex, imprimer un message d'info sur 'ERREUR'.
        'cc' : liste de noms de fichiers ou objets fichier dans lesquels copier le message
        """
        if len(self._buffer) < 1:
            return None

        # construction du dictionnaire du message global
        dglob = {
            'flags'         : self.get_current_flags(),
            'id_message'    : self.get_current_id(),
            'liste_message' : [],
            'liste_context' : [],
        }
        dglob['code'], dglob['exc_name'], dglob['exc_typ'] = self.get_current_code()
        for dictmess in self._buffer:
            dglob['liste_message'].append(dictmess['corps_message'])
            dglob['liste_context'].append(dictmess['context_info'])
        dglob['corps_message'] = ''.join(dglob['liste_message'])
        dglob['context_info'] = ''.join(dglob['liste_context'])

        # liste des unités d'impression en fonction du type de message
        if dglob['flags'] & ALL_UNIT:
            print_as = 'E'
        l_unit = list_unit(print_as or dglob['code'])

        # texte final et impression
        txt = self.format_message(dglob)
        for unite in l_unit:
            self.affiche(unite, txt)
        # "cc"
        if cc:
            copy_text_to(convert(txt), cc)

        self.init_buffer()

    def disable_alarm(self, idmess, hide=False):
        """Ignore l'alarme "idmess".
        """
        idmess = idmess.strip()
        if hide:
            self._hidden_alarm[idmess] = self._hidden_alarm.get(idmess, 0) + 1
        else:
            self._ignored_alarm[idmess] = self._ignored_alarm.get(idmess, 0) + 1

    def reset_alarm(self, idmess, hide=False):
        """Réactive l'alarme "idmess".
        """
        idmess = idmess.strip()
        if hide:
            self._hidden_alarm[idmess] = min(self._hidden_alarm.get(idmess, 0) - 1, 0)
        else:
            self._ignored_alarm[idmess] = min(self._ignored_alarm.get(idmess, 0) - 1, 0)

    def is_alarm_disabled(self, idmess):
        """Doit-on ignorer l'alarme "idmess" ?
        """
        return self._ignored_alarm.get(idmess, 0) + self._hidden_alarm.get(idmess, 0) > 0

    def get_info_alarm(self, only_ignored=False):
        """Retourne la liste des alarmes émises, le nombre d'occurrence
        pour chacune d'elle et un indicateur disant si elle a été masquée ou pas."""
        s_alarm = set(self._ignored_alarm.keys())
        if not only_ignored:
            s_alarm.update(self.count_alarm_tot.keys())
        l_all = list(s_alarm)
        l_all.sort()
        # occurrences
        l_alarm, l_occ, l_masq = [], [], []
        for idmess in l_all:
            nb = self.count_alarm_tot.get(idmess, 0)
            if nb > 0:
                l_alarm.append(idmess)
                l_occ.append(nb)
                l_masq.append(int(self._ignored_alarm.get(idmess) is not None))
        return zip(l_alarm, l_occ, l_masq)

    def get_info_alarm_nb(self, only_ignored=False):
        """Retourne le nombre d'alarme émises (et non masquées)."""
        res = self.get_info_alarm(only_ignored)
        res = [item for item in res if item[2] == 0]
        return len(res)

    def info_alarm(self, only_ignored=False):
        """Fournit les infos sur les alarmes activées.
        """
        # on sépare des éventuels messages en attente
        self.print_buffer_content()
        # entete
        dictmess = self.get_message('I', 'CATAMESS_89')
        self.add_to_buffer(dictmess)
        # occurrences
        res = self.get_info_alarm(only_ignored)
        for idmess, nb, masq in res:
            mark = ' ' + '(*)' * masq
            dictmess = self.get_message('I', 'CATAMESS_90', valk=(mark, idmess), vali=nb)
            self.add_to_buffer(dictmess)
        if not res:
            dictmess = self.get_message('I', 'CATAMESS_92')
            self.add_to_buffer(dictmess)
        self.print_buffer_content()

    def update_counter(self):
        """Mise à jour des compteurs et réaction si besoin.
        """
        code = self.get_current_code()[0]
        if   code == 'E':
            self.erreur_E = True
        elif code == 'F':
            self.erreur_E = False
        elif code == 'A':
            idmess = self.get_current_id().strip()
            # nombre d'occurrence de cette alarme (sauf si cachee)
            if self._hidden_alarm.get(idmess, 0) == 0:
                self.count_alarm[idmess]     = self.count_alarm.get(idmess, 0) + 1
                self.count_alarm_tot[idmess] = self.count_alarm_tot.get(idmess, 0) + 1

            if self.is_alarm_disabled(idmess) or self.count_alarm[idmess] > self.nmax_alarm:
                # ignorer l'alarme ou count_alarm > max, on vide le buffer
                self.init_buffer()
            elif self.count_alarm[idmess] == self.nmax_alarm:
                # Pour mettre en relief le message CATAMESS_41, on le sépare
                # de la dernière alarme
                self.print_buffer_content()
                dictmess = self.get_message(code, 'CATAMESS_41',
                                        valk=idmess, vali=self.nmax_alarm)
                self.add_to_buffer(dictmess)

    def check_counter(self, info_alarm=0, silent=0):
        """Méthode "jusqu'ici tout va bien" ! (Interface C : chkmsg)
        Si des erreurs <E> se sont produites, on arrete le code en <F>.
        Appelée par FIN ou directement au cours de l'exécution d'une commande.
        Retourne un entier : 0 si tout est ok.
        Si silent==1, on n'émet pas de message, on ne s'arrete pas.
        """
        iret = 0
        if self.erreur_E:
            iret = 4
            self.erreur_E = False
            if not silent:
                self.print_message('F', 'CATAMESS_6', exception=True)
        if info_alarm:
            self.info_alarm()
        return iret

    def reset_command(self):
        """Méthode appelée entre les commandes. (Interface C : resmsg)
        On remet à zéro le compteur d'alarme,
        on vérifie les erreurs <E> en attente."""
        iret = self.check_counter()
        # reset des alarmes
        self.count_alarm = {}
        # reset du cache, sans doute inutile car l'ensemble des messages représente
        # environ 1 Mo.
        if len(self._cache_txt) > 1000:
            self._cache_txt = {}

    def format_message(self, dictmess):
        """Formate le message décrit dans un dico :
            'code'          : A, E, S, F, I
            'id_message'    : identification du message
            'corps_message' : texte
        """
        charh = '-'    # horizontal
        charv = '!'    # vertical
        charc = '!'    # coin
        dcomm = {
            'A' : _(u"""Ceci est une alarme. Si vous ne comprenez pas le sens de cette
alarme, vous pouvez obtenir des résultats inattendus !"""),
            'E' : _(u"""Cette erreur sera suivie d'une erreur fatale."""),
            'S' : _(u"""Cette erreur est fatale. Le code s'arrête. Toutes les étapes
du calcul ont été sauvées dans la base jusqu'au moment de l'arret."""),
            'F' : _(u"""Cette erreur est fatale. Le code s'arrête."""),
        }

        # format complet
        format_general = {
            'decal'  : u'   ',
            'header' : u"""<%(type_message)s> %(str_id_message)s""",
            'ligne'  : u'%(charv)s %%-%(maxlen)ds %(charv)s',
            'corps'  : u"""%(header)s

%(corps_message)s
%(context_info)s

%(commentaire)s
""",
            'final'  : u"""
%(separateur)s
%(corps)s
%(separateur)s

""",
        }
        # format light pour les infos
        format_light = {
            'decal'  : u'',
            'header' : u"""<%(type_message)s> """,
            'ligne'  : u'%%s',
            'corps'  : u"""%(corps_message)s
%(context_info)s""",
            'final'  : u"""%(corps)s""",
        }
        dmsg = dictmess.copy()
        dmsg['type_message'] = self.get_type_message(dictmess)
        if dmsg['id_message'] != 'I':
            dmsg['str_id_message'] = '<%s>' % dmsg['id_message']
        else:
            dmsg['str_id_message'] = ''

        # format utilisé
        format = format_general
        if dmsg['type_message'] == 'I':
            format = format_light
        if dmsg['flags'] & DECORATED:
            format = format_general
        if format is format_general:
            lines = dmsg['corps_message'].splitlines()
            while len(lines) > 1 and lines[0].strip() == '':
                del lines[0]
            dmsg['corps_message'] = os.linesep.join(lines).rstrip()

        dmsg['header'] = ufmt(format['header'], dmsg)
        dmsg['commentaire'] = dcomm.get(dmsg['type_message'], '')
        if re.search('^DVP', dmsg['id_message']) != None:
            dmsg['commentaire'] += contacter_assistance

        dmsg['corps'] = ufmt(format['corps'], dmsg)
        if format is format_general:
            dmsg['corps'] = dmsg['corps'].strip()


        # longueur de la ligne la plus longue
        l_line = dmsg['corps'].splitlines()
        maxlen = max([len(line) for line in l_line])

        # format des lignes sur maxlen caractères
        dlin = {
            'charh'  : charh,
            'charv'  : charv,
            'charc'  : charc,
            'maxlen' : maxlen
        }
        fmt_line = ufmt(format['ligne'], dlin)

        # on formate toutes les lignes
        if dmsg['flags'] & CENTER:
            l_line = [line.strip().center(LINE_WITH) for line in l_line]
        txt = [fmt_line % line for line in l_line]
        dmsg['corps'] = os.linesep.join(txt)
        dmsg['separateur'] = charc + charh * (maxlen + 2) + charc

        # ligne haut et bas
        newtxt = format['final'] % dmsg
        # on décale
        l_txt = [format['decal'] + line for line in newtxt.splitlines()]

        return clean_string(os.linesep.join(l_txt))


    def get_type_message(self, dictmess):
        """Retourne le type du message affiché.
        En cas d'erreur, si on lève une exception au lieu de s'arreter,
        on affiche le type de l'erreur.
        """
        code = dictmess['code']
        typmess = code.strip()
        if self.onFatalError().startswith('EXCEPTION'):
            if typmess in ('E', 'F'):
                typmess = 'EXCEPTION'
        if typmess == 'D':
            typmess = 'F'
        # dans tous les cas, pour S et Z (exception), on affiche EXCEPTION.
        elif code == 'S':
            typmess = 'EXCEPTION'
        elif code == 'Z':
            typmess = dictmess.get('exc_name') or 'EXCEPTION'
        return typmess

    def get_context(self, ctxt_msg, idmess, dicarg):
        """Prise en compte du context du message pour donner d'autres infos
        à l'utilisateur.
        ctxt_msg est un dictionnaire. Les clés traitées sont :
            - CONCEPT
        """
        if not ctxt_msg:
            return ''
        msg = []
        # tout dans un try/except car c'est du bonus, il ne faudrait pas planter !
        try:
            if ctxt_msg.has_key('CONCEPT'):
                l_co =  [dicarg[arg] for arg in force_list(ctxt_msg['CONCEPT'])]
                for co in l_co:
                    msg.append(message_context_concept(co))
        except:
            pass
        return os.linesep.join(msg)

    # définitions pour fonctionner sans le module aster
    def affiche(self, unite, txt):
        """Affichage du message"""
        txt = convert(txt)
        if aster_exists:
            aster.affiche(unite, txt)
        else:
            print txt

    def onFatalError(self):
        """Récupérer le comportement en cas d'erreur fatale."""
        if aster_exists:
            return aster.onFatalError()
        else:
            return 'EXCEPTION'


def raise_UTMESS(exc):
    """Raise UTMESS if exception occurred.

    Typical usage:
        try:
            ... code with error ...
            raise aster.error(id_message, valk, vali, valr)
            ... or ...
            lerr = [id_message1, valk1, vali1, valr1]
            lerr.append([id_message2, valk2, vali2, valr2])
            raise aster.error(lerr)
        except aster.error, exc:
            raise_UTMESS(exc)

    """
    for err in exc.related:
        UTMESS('F+', err.id_message, valk=err.valk, vali=err.vali, valr=err.valr)
    UTMESS('F', exc.id_message, valk=exc.valk, vali=exc.vali, valr=exc.valr)


# unique instance du MESSAGE_LOGGER
MessageLog = MESSAGE_LOGGER()

def UTMESS(code, idmess, valk=(), vali=(), valr=(), exc_num=None, print_as=None, cc=None):
    """Utilitaire analogue à la routine fortran U2MESS/U2MESG avec les arguments
    optionnels.
        code   : 'A', 'E', 'S', 'F', 'I'
        idmess : identificateur du message
        valk, vali, valr : liste des chaines, entiers ou réels.

    Appel sans valeurs :                avec valeurs :
        UTMESS('A', 'SUPERVIS_40')          UTMESS('A', 'SUPERVIS_40', vali=[1, 2])

    Remarques :
        - Nommer les arguments permet de ne pas tous les passer.
        - Meme fonctionnement que U2MESG :
            + appel à MessageLog
            + puis exception ou abort en fonction du niveau d'erreur.
    """
    MessageLog(code, idmess, valk, vali, valr, exc_num=exc_num,
               exception=True, print_as=print_as, cc=cc)


def ASSERT(condition, message=""):
    """Remonter un assert dans un message.
    """
    if condition:
        return
    stack = traceback.format_stack(limit=10)
    UTMESS('F', 'DVP_9', valk=[(''.join(stack[:-1]),), message])


def message_exception(code, exc):
    """Retourne le message associé à une exception aster.error
    tel qu'il aurait été imprimé par UTMESS selon la valeur de
    `code` ('I', 'A', 'S', 'F', 'Z'...)."""
    return MessageLog.GetText(code, exc.id_message,
                              exc.valk, exc.vali, exc.valr)


def MasquerAlarme(idmess):
    """Masque une alarme : ni affichee, ni comptee.
    Utilisation dans les macros :
        MasquerAlarme(XXX)  au debut de la macro
        RetablirAlarme(XXX) a la fin de la macro
    Comme il s'agit d'un compteur qui est incremente puis decremente, il est
    imperatif qu'il y ait autant d'appel a MasquerAlarme qu'a RetablirAlarme.
    """
    MessageLog.disable_alarm(idmess, hide=True)


def RetablirAlarme(idmess):
    """Retablit l'etat initial pour l'alarme 'idmess'.
    """
    MessageLog.reset_alarm(idmess, hide=True)


# faux appels à UTMESS
def __fake__():
    UTMESS('I', 'SUPERVIS_40')    # surcharge émis par asrun
    UTMESS('I', 'SUPERVIS_96')    # émis depuis le C (inisig)
    UTMESS('I', 'SUPERVIS_97')    # émis depuis le C (inisig)
    UTMESS('I', 'GENERIC_1')      # dans des tests pour traiter les exceptions
    UTMESS('I', 'CATAMESS_55')    # pour u2mesg.f via UTPRIN
    UTMESS('I', 'CATAMESS_69')    # pour u2mesg.f via UTPRIN
    UTMESS('I', 'CATAMESS_70')    # pour u2mesg.f via UTPRIN
    # message.info/error/warn
    UTMESS('I', 'SUPERVIS2_1')    # émis dans ops
    # utilisé ici
    UTMESS('I', 'CATAMESS_6')
    UTMESS('I', 'CATAMESS_41')
    UTMESS('I', 'CATAMESS_57')
    UTMESS('I', 'CATAMESS_89')
    UTMESS('I', 'CATAMESS_90')
    UTMESS('I', 'CATAMESS_92')
    # appelé par levé d'exception
    # dans Miss/*.py
    UTMESS('I', 'MISS0_3')
    UTMESS('I', 'MISS0_5')
    UTMESS('I', 'MISS0_6')
    UTMESS('I', 'MISS0_7')
    UTMESS('I', 'MISS0_8')
    UTMESS('I', 'MISS0_9')
    UTMESS('I', 'MISS0_11')
    UTMESS('I', 'MISS0_17')
    # dans TableReader.py
    UTMESS('I', 'TABLE0_10')
    UTMESS('I', 'TABLE0_11')
    UTMESS('I', 'TABLE0_12')
    UTMESS('I', 'TABLE0_13')
    UTMESS('I', 'TABLE0_15')
    UTMESS('I', 'TABLE0_43')


