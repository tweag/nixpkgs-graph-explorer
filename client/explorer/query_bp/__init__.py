from flask import request, Response, render_template, Blueprint, session
from explorer.utils import utils

# Blueprint Configuration
query_bp = Blueprint(
    'query_bp', __name__,
    template_folder='templates',
)

@query_bp.get("/")
def index_get():
    return render_template('query.html', dest='/')

@query_bp.post("/")
def index_post():
    result = utils.do_things(request.form['query'])
    # session['graph-data'] = utils.load_json('https://js.cytoscape.org/demos/colajs-graph/data.json')
    # resp = Response(result)
    # resp.headers['HX-Redirect'] = '/dashapp'
    return render_template('query_result.html', result=result)
